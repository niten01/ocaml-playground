#version 330

// Input vertex attributes (from vertex shader)
in vec3 fragPosition;
in vec2 fragTexCoord;
in vec4 fragColor;
in vec3 fragNormal;

// Input uniform values
uniform sampler2D texture0;
uniform vec4 colDiffuse;

// Output fragment color
out vec4 finalColor;

#define     MAX_LIGHTS              10 
#define     LIGHT_POINT             0
#define     LIGHT_DIRECTIONAL       1
#define     LIGHT_AMBIENT 0.03
#define     LIGHT_DIFFUSE 1.0 
#define     LIGHT_SPECULAR 1.0

struct Light {
    int enabled;
    int type;
    float strength;
    vec3 position;
    vec3 target;
    vec3 color;
};

// Input lighting values
uniform Light lights[MAX_LIGHTS];
uniform vec3 viewPos;

vec3 calc_dir_light(Light light, vec3 normal, vec3 viewDir, vec3 diffuseBase);
vec3 calc_point_light(Light light, vec3 normal, vec3 fragPos, vec3 viewDir, vec3 diffuseBase);

void main()
{
    // Texel color fetching from texture sampler
    vec4 texelColor = texture(texture0, fragTexCoord);

    vec3 lightDot = vec3(0.0);
    vec3 normal = normalize(fragNormal);
    vec3 viewD = normalize(fragPosition - viewPos);
    vec3 specular = vec3(0.0);

    // vec4 tint = colDiffuse * fragColor;
    vec4 tint = texelColor;
    vec3 result = vec3(0);

    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        if (lights[i].enabled == 1)
        {
            vec3 light = vec3(0.0);

            if (lights[i].type == LIGHT_DIRECTIONAL)
            {
                // result += calc_dir_light(lights[i], normal, viewD, vec3(texelColor));
                light = -normalize(lights[i].target - lights[i].position);
            }

            if (lights[i].type == LIGHT_POINT)
            {
                // result += calc_point_light(lights[i], normal, fragPosition, viewD, vec3(texelColor));
                light = normalize(lights[i].position - fragPosition);
            }

            float NdotL = max(dot(normal, light), 0.0);
            float distance = distance(lights[i].position, fragPosition);
            float distCoeff = 1.0/(distance*distance);
            if (lights[i].type == LIGHT_DIRECTIONAL)
            {
                distCoeff = 1.0;
            }
            lightDot += lights[i].color.rgb*NdotL*distCoeff*lights[i].strength;

            float specCo = 0.0;
            if (NdotL > 0.0) specCo = pow(max(0.0, dot(viewD, reflect(-(light), normal))), 16.0); // 16 refers to shine
            specular += specCo * lights[i].strength;
        }
    }

    finalColor = (texelColor*((tint + vec4(specular, 1.0))*vec4(lightDot, 1.0)));
    // finalColor += texelColor*(vec4(LIGHT_AMBIENT)/10.0)*tint;

    // Gamma correction
    finalColor.rgb = pow(finalColor.rgb, vec3(1.0/2.2));
}


vec3 calc_dir_light(Light light, vec3 normal, vec3 viewDir, vec3 diffuseBase) {	
	vec3 lightDir = normalize(light.target - light.position);
	vec3 halfway = normalize(-lightDir - viewDir);

	// diffuse
	float diff = max(dot(normal, -lightDir), 0.0); // negate to match normal dir

	//specular
	vec3 reflectDir = reflect(lightDir, normal); //actual reflect
	float spec = pow(max(dot(halfway, normal), 0.0), 16.0);

	vec3 ambient = LIGHT_AMBIENT * light.color * diffuseBase;
	vec3 diffuse = LIGHT_DIFFUSE *  light.color * light.strength * (diff * diffuseBase);

	vec3 specular = light.strength * LIGHT_SPECULAR * (spec * diffuseBase);

	return (ambient + diffuse + specular);
}

vec3 calc_point_light(Light light, vec3 normal, vec3 fragPos, vec3 viewDir, vec3 diffuseBase) {
	vec3 lightDir = normalize(fragPos - light.position);
	vec3 halfway = normalize(-lightDir - viewDir);

	// diffuse
	float diff = max(dot(normal, -lightDir), 0.0); // negate to match normal dir

	//specular
	vec3 reflectDir = reflect(lightDir, normal); //actual reflect
	float spec = pow(max(dot(halfway, normal), 0.0), 16.0);

	float constant = 1.;
	float linear = .09;
	float quadratic = .032;

	//attenuation
	float distance = length(light.position - fragPos);
float attenuation =  1.0 / (quadratic*(distance*distance) +
						   linear*distance + 
						   constant);
	// float attenuation = 1.0 / (distance*distance);


	vec3 ambient =  LIGHT_AMBIENT * light.color * attenuation * diffuseBase;
	vec3 diffuse =  LIGHT_DIFFUSE * light.color * light.strength * attenuation * (diff * diffuseBase);
	vec3 specular = light.strength * attenuation * LIGHT_SPECULAR *  vec3(1) * (spec * diffuseBase);

	return (ambient + diffuse + specular);
}
