#version 330

// Input vertex attributes (from vertex shader)
in vec2 fragTexCoord;
in vec4 fragColor;

// Input uniform values
uniform sampler2D texture0;
uniform vec4 colDiffuse;

// Output fragment color
out vec4 finalColor;

uniform float renderWidth;
uniform float renderHeight;

float gamma = 0.6;

vec4 stitch(sampler2D tex, vec2 uv)
{
    float stitchingSize = 6.0;
    int invert = 0;

    vec4 c = vec4(0.0);
    float size = stitchingSize ;
    vec2 cPos = uv * vec2(renderWidth, renderHeight);
    vec2 tlPos = floor(cPos / vec2(size, size));
    tlPos *= size;

    int remX = int(mod(cPos.x, size));
    int remY = int(mod(cPos.y, size));

    if (remX == 0 && remY == 0) tlPos = cPos;

    vec2 blPos = tlPos;
    blPos.y += (size - 1.0);

    if ((remX == remY) || (((int(cPos.x) - int(blPos.x)) == (int(blPos.y) - int(cPos.y)))))
    {
        if (invert == 1) c = vec4(0.2, 0.15, 0.05, 1.0);
        else c = texture(tex, tlPos * vec2(1.0/renderWidth, 1.0/renderHeight)) * 1.4;
    }
    else
    {
        if (invert == 1) c = texture(tex, tlPos * vec2(1.0/renderWidth, 1.0/renderHeight)) * 1.4;
        else c = vec4(0.0, 0.0, 0.0, 1.0);
    }

    return c;
}

vec4 edges(sampler2D tex, vec2 uv) {
    float x = 1.0/renderWidth;
    float y = 1.0/renderHeight;

    vec4 horizEdge = vec4(0.0);
    horizEdge -= texture(texture0, vec2(fragTexCoord.x - x, fragTexCoord.y - y))*1.0;
    horizEdge -= texture(texture0, vec2(fragTexCoord.x - x, fragTexCoord.y    ))*2.0;
    horizEdge -= texture(texture0, vec2(fragTexCoord.x - x, fragTexCoord.y + y))*1.0;
    horizEdge += texture(texture0, vec2(fragTexCoord.x + x, fragTexCoord.y - y))*1.0;
    horizEdge += texture(texture0, vec2(fragTexCoord.x + x, fragTexCoord.y    ))*2.0;
    horizEdge += texture(texture0, vec2(fragTexCoord.x + x, fragTexCoord.y + y))*1.0;

    vec4 vertEdge = vec4(0.0);
    vertEdge -= texture(texture0, vec2(fragTexCoord.x - x, fragTexCoord.y - y))*1.0;
    vertEdge -= texture(texture0, vec2(fragTexCoord.x    , fragTexCoord.y - y))*2.0;
    vertEdge -= texture(texture0, vec2(fragTexCoord.x + x, fragTexCoord.y - y))*1.0;
    vertEdge += texture(texture0, vec2(fragTexCoord.x - x, fragTexCoord.y + y))*1.0;
    vertEdge += texture(texture0, vec2(fragTexCoord.x    , fragTexCoord.y + y))*2.0;
    vertEdge += texture(texture0, vec2(fragTexCoord.x + x, fragTexCoord.y + y))*1.0;

    vec3 edge = sqrt((horizEdge.rgb*horizEdge.rgb) + (vertEdge.rgb*vertEdge.rgb));
    edge = vec3((edge.r+edge.g+edge.b)/3.0);

    if(edge.r < 0.5) {
      return vec4(0.0);
    }

    vec4 c = vec4(edge, texture(texture0, fragTexCoord).a);
    return c;
}

vec4 bloom(sampler2D tex, vec2 uv) {
    float samples = 5.0;          // Pixels per axis; higher = bigger glow, worse performance
    float quality = 2.5;          // Defines size factor: Lower = smaller glow, better quality

    vec2 size = vec2(renderWidth, renderHeight);
    vec4 sum = vec4(0);
    vec2 sizeFactor = vec2(1)/size*quality;

    // Texel color fetching from texture sampler
    vec4 source = texture(tex, uv);

    const int range = 2;            // should be = (samples - 1)/2;

    for (int x = -range; x <= range; x++)
    {
        for (int y = -range; y <= range; y++)
        {
            sum += texture(tex, uv + vec2(x, y)*sizeFactor);
        }
    }

    // Calculate final fragment color
    vec4 c  = ((sum/(samples*samples)) + source)*colDiffuse*vec4(0.6);
    return c;
}

vec4 posterize(vec4 color) {
    float numColors = 8.0;

    vec4 c = color; 
    c = c*numColors;
    c = floor(c);
    c = c/numColors;
    return c;
}

void main()
{
    vec4 baseColor = bloom(texture0, fragTexCoord);

    float avg = (baseColor.r+baseColor.g+baseColor.b)/3.0;
    baseColor = mix(baseColor, vec4(avg,avg,avg,1.0), 0.35);

    baseColor = posterize(baseColor);

    vec4 stitchedColor = stitch(texture0, fragTexCoord);
    finalColor = mix(baseColor, stitchedColor, 0.08);
}
