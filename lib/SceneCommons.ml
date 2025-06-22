open Raylib

let interaction_distance = 6.0
let flashlight_tag = "flashlight"

type t = {
  objects : Object.t list;
  lighting : LightingSystem.t;
  player : Player.t;
  postprocess_shader : Shader.t;
  ambient_music : Music.t;
  start_anim : UIAnim.t;
  can_interact : bool;
  interact_texture : Texture2D.t;
}

let (render_texture : RenderTexture.t option ref) = ref None
let (text_font : Font.t option ref) = ref None

let prevent_player_collision objects player =
  let player = player in
  if List.exists (fun obj -> Object.collides_with player obj) objects then
    Player.undo_movement player
  else player

let draw_interaction_texture (scene : t) =
  let w, h =
    ( float @@ Texture2D.width scene.interact_texture,
      float @@ Texture2D.height scene.interact_texture )
  in
  let scale = 0.2 in
  draw_texture_ex scene.interact_texture
    (Vector2.create
       ((float @@ (get_screen_width () / 2)) -. w *. scale *. 0.5)
       ((float @@ (get_screen_height () / 2)) -. h *. scale *. 0.5))
    0. scale Color.white

let create objects lighting player postprocess_shader_path ambient_music_path =
  let lighting =
    LightingSystem.add_tagged_point_light flashlight_tag
      (Player.position player) 0.04 Color.white lighting
  in
  let shader = LightingSystem.shader lighting in
  List.iter (fun obj -> Object.apply_shader shader obj) objects;
  let postprocess_shader = load_shader "" postprocess_shader_path in
  let render_width_loc = get_shader_location postprocess_shader "renderWidth" in
  let render_height_loc =
    get_shader_location postprocess_shader "renderHeight"
  in
  let render_width_ptr =
    to_voidp @@ Ctypes.allocate Ctypes.float (float @@ get_screen_width ())
  in
  let render_height_ptr =
    to_voidp @@ Ctypes.allocate Ctypes.float (float @@ get_screen_height ())
  in
  set_shader_value postprocess_shader render_width_loc render_width_ptr
    ShaderUniformDataType.Float;
  set_shader_value postprocess_shader render_height_loc render_height_ptr
    ShaderUniformDataType.Float;
  let ambient_music = load_music_stream ambient_music_path in
  Music.set_looping ambient_music true;
  play_music_stream ambient_music;
  let start_anim =
    UIAnim.create "resources/textures/eye_anim.png" 180 4 |> UIAnim.start
  in
  let interact_texture = load_texture "resources/textures/interact_icon.png" in
  {
    objects;
    lighting;
    player;
    postprocess_shader;
    ambient_music;
    start_anim;
    can_interact = false;
    interact_texture;
  }

let destroy (data : t) =
  List.iter (fun o -> Object.destroy o) data.objects;
  LightingSystem.destroy data.lighting;
  Player.destroy data.player;
  unload_shader data.postprocess_shader;
  unload_music_stream data.ambient_music;
  UIAnim.destroy data.start_anim;
  unload_texture data.interact_texture

let init () =
  render_texture :=
    Some (load_render_texture (get_screen_width ()) (get_screen_height ()));
  text_font := Some (load_font "resources/fonts/Times New Roman.ttf")

let get_render_texture () =
  match !render_texture with
  | None -> failwith "render texture not initialized"
  | Some rt -> rt

let get_text_font () =
  match !text_font with None -> failwith "text font not loaded" | Some f -> f

let draw (scene : t) =
  let lighting = scene.lighting in
  let player = scene.player in

  LightingSystem.begin_system lighting;
  LightingSystem.update_shader lighting (Player.get_view player);
  List.iter Object.draw scene.objects;
  LightingSystem.end_system ()
  (* List.iter (fun o -> draw_bounding_box (Object.bbox o) Color.red) scene.objects *)

let render_to_texture (draw_f : unit -> unit) scene =
  begin_texture_mode (get_render_texture ());
  let native_camera = Player.get_view scene.player in
  begin_mode_3d native_camera;
  draw_f ();
  end_mode_3d ();
  end_texture_mode ()

let render_to_screen (draw_overlay_f : unit -> unit) scene =
  begin_drawing ();
  clear_background Color.raywhite;

  begin_shader_mode scene.postprocess_shader;
  let texture2d = RenderTexture.texture @@ get_render_texture () in
  draw_texture_rec texture2d
    (Rectangle.create 0. 0.
       (float @@ Texture2D.width texture2d)
       (float @@ -Texture2D.height texture2d))
    (Vector2.zero ()) Color.white;

  end_shader_mode ();
  draw_overlay_f ();
  Player.draw_2d scene.player;
  if scene.can_interact then draw_interaction_texture scene;

  UIAnim.draw
    (Rectangle.create (-8.) 0.
       (float @@ (get_screen_width () + 8))
       (float @@ get_screen_height ()))
    scene.start_anim;
  end_drawing ()

let update (scene : t) =
  let player =
    Player.update scene.player |> prevent_player_collision scene.objects
  in
  update_music_stream scene.ambient_music;
  let start_anim = UIAnim.update scene.start_anim in
  let lighting =
    LightingSystem.set_light_position flashlight_tag (Player.position player)
      scene.lighting
  in
  { scene with player; start_anim; lighting }

let get_screen_to_world_ray (scene : t) =
  let camera = Player.get_view scene.player in
  let cam_pos = Camera3D.position camera in
  let cam_fwd = FPCamera.forward_norm @@ Player.fpcamera scene.player in
  Ray.create cam_pos cam_fwd

let interacted bbox common =
  let look_ray = get_screen_to_world_ray common in
  let col = get_ray_collision_box look_ray bbox in
  let can_interact =
    RayCollision.hit col && RayCollision.distance col < interaction_distance
  in
  ( is_mouse_button_pressed MouseButton.Left && can_interact,
    { common with can_interact } )
