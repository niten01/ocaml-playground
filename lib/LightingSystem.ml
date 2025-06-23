open Raylib

module LightType = struct
  type t = Point | Directional

  let to_int t = match t with Point -> 0 | Directional -> 1
end

module Light = struct
  type t = {
    tag : string;
    enabled : bool;
    light_type : LightType.t;
    strength : float;
    position : Vector3.t;
    target : Vector3.t option;
    color : Color.t;
    enabled_loc : ShaderLoc.t;
    light_type_loc : ShaderLoc.t;
    strength_loc : ShaderLoc.t;
    position_loc : ShaderLoc.t;
    target_loc : ShaderLoc.t;
    color_loc : ShaderLoc.t;
  }

  let tag t = t.tag

  let default =
    {
      tag = "";
      enabled = false;
      light_type = LightType.Point;
      strength = 0.0;
      position = Vector3.zero ();
      target = None;
      color = Color.white;
      enabled_loc = 0;
      light_type_loc = 0;
      strength_loc = 0;
      position_loc = 0;
      target_loc = 0;
      color_loc = 0;
    }
end

type t = { lights : Light.t array; light_count : int; shader : Shader.t }

let max_lights = 10
let vec4_from_accessors f a b c d = Vector4.create (f a) (f b) (f c) (f d)

let color_to_vec4 color =
  vec4_from_accessors
    (fun acc -> (float @@ acc color) /. 255.)
    Color.r Color.g Color.b Color.a

let create () =
  let shader =
    load_shader "resources/shaders/lighting.vs" "resources/shaders/lighting.fs"
  in
  let ambient_loc = get_shader_location shader "ambient" in
  let view_pos_loc = get_shader_location shader "viewPos" in
  set_shader_value shader ambient_loc
    (to_voidp @@ addr @@ color_to_vec4 Color.black)
    ShaderUniformDataType.Vec4;
  Shader.set_loc shader ShaderLocationIndex.Vector_view view_pos_loc;
  if Shader.id shader = Unsigned.UInt.zero then
    failwith "Failed to compile lighting shader";
  { lights = Array.make max_lights Light.default; light_count = 0; shader }

let destroy system = unload_shader system.shader

let create_tagged_point_light tag position strength color system =
  let get_loc_name loc =
    Printf.sprintf "lights[%i].%s" system.light_count loc
  in
  let get_loc loc = get_shader_location system.shader (get_loc_name loc) in
  let light =
    {
      tag;
      Light.enabled = true;
      light_type = LightType.Point;
      strength;
      position;
      target = None;
      color;
      enabled_loc = get_loc "enabled";
      light_type_loc = get_loc "type";
      strength_loc = get_loc "strength";
      position_loc = get_loc "position";
      target_loc = get_loc "target";
      color_loc = get_loc "color";
    }
  in
  light

let add_tagged_point_light tag position strength color system =
  let light = create_tagged_point_light tag position strength color system in
  system.lights.(system.light_count) <- light;
  { system with light_count = system.light_count + 1 }

let add_point_light position strength color system =
  add_tagged_point_light "" position strength color system

let add_dir_light position target strength color system =
  let point_light =
    create_tagged_point_light "" position strength color system
  in
  let light =
    {
      point_light with
      light_type = LightType.Directional;
      target = Some target;
    }
  in
  system.lights.(system.light_count) <- light;
  { system with light_count = system.light_count + 1 }

let shader system = system.shader

let set_light_position tag position system =
  let lights =
    Array.map
      (fun l -> if Light.tag l = tag then { l with position } else l)
      system.lights
  in
  { system with lights }

let update_shader system camera =
  let shader = system.shader in
  let view_pos_loc =
    CArray.get (Shader.locs shader)
      (ShaderLocationIndex.to_int ShaderLocationIndex.Vector_view)
  in
  set_shader_value shader view_pos_loc
    (camera |> Camera3D.position |> addr |> to_voidp)
    ShaderUniformDataType.Vec3;
  for i = 0 to system.light_count - 1 do
    let light = system.lights.(i) in
    let enabled_ptr = to_voidp @@ Ctypes.allocate Ctypes.bool light.enabled in
    set_shader_value shader light.enabled_loc enabled_ptr
      ShaderUniformDataType.Int;
    let light_type_ptr =
      to_voidp @@ Ctypes.allocate Ctypes.int (LightType.to_int light.light_type)
    in
    let strength_ptr =
      to_voidp @@ Ctypes.allocate Ctypes.float light.strength
    in
    set_shader_value shader light.strength_loc strength_ptr
      ShaderUniformDataType.Float;
    set_shader_value shader light.light_type_loc light_type_ptr
      ShaderUniformDataType.Int;
    set_shader_value shader light.position_loc
      (to_voidp @@ addr light.position)
      ShaderUniformDataType.Vec3;
    set_shader_value shader light.target_loc
      (to_voidp @@ addr @@ Option.value light.target ~default:(Vector3.zero ()))
      ShaderUniformDataType.Vec3;
    set_shader_value shader light.color_loc
      (to_voidp @@ addr @@ color_to_vec4 light.color)
      ShaderUniformDataType.Vec4
  done

let begin_system system =
  (* for i = 0 to system.light_count - 1 do *)
  (*   draw_sphere system.lights.(i).position 1. system.lights.(i).color *)
  (* done; *)
  begin_shader_mode system.shader

let end_system () = end_shader_mode ()
