open Raylib

let interaction_distance = 6.0
let flashlight_tag = "flashlight"
let mute_volume = 0.1
let mute_fade_in_frames = 120

type t = {
  objects : Object.t list;
  sky : Object.t option;
  lighting : LightingSystem.t;
  player : Player.t;
  postprocess_shader : Shader.t;
  ambient_music : Music.t;
  ambient_mute_frame_counter : int;
  start_anim : UIAnim.t;
  mutable can_interact_ui : bool;
  interact_texture : Texture2D.t;
}

let (render_texture : RenderTexture.t option ref) = ref None
let (text_font : Font.t option ref) = ref None

let prevent_player_collision objects player =
  let player = player in
  if List.exists (fun obj -> Object.collides_with player obj) objects then
    Player.undo_movement player
  else player

let mute_for seconds common =
  set_music_volume common.ambient_music mute_volume;
  { common with ambient_mute_frame_counter = seconds * 60 }

let draw_interaction_texture (scene : t) =
  let w, h =
    ( float @@ Texture2D.width scene.interact_texture,
      float @@ Texture2D.height scene.interact_texture )
  in
  let scale = 0.2 in
  draw_texture_ex scene.interact_texture
    (Vector2.create
       ((float @@ (get_screen_width () / 2)) -. (w *. scale *. 0.5))
       ((float @@ (get_screen_height () / 2)) -. (h *. scale *. 0.5)))
    0. scale Color.white

let create objects lighting player postprocess_shader_path ambient_music_path =
  let lighting =
    LightingSystem.add_tagged_point_light flashlight_tag
      (Player.position player) 0.5 Color.white lighting
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
    sky = None;
    lighting;
    player;
    postprocess_shader;
    ambient_music;
    ambient_mute_frame_counter = 0;
    start_anim;
    can_interact_ui = false;
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

let set_sky sky scene = { scene with sky = Some sky }

let draw (scene : t) =
  let lighting = scene.lighting in
  let player = scene.player in

  LightingSystem.begin_system lighting;
  LightingSystem.update_shader lighting (Player.get_view player);
  List.iter Object.draw scene.objects;
  if Option.is_some scene.sky then Object.draw @@ Option.get scene.sky;
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
  if scene.can_interact_ui then draw_interaction_texture scene;

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
  let sky =
    match scene.sky with
    | None -> scene.sky
    | Some sky_obj ->
        Some (sky_obj |> Object.set_position @@ Player.position player)
  in
  update_music_stream scene.ambient_music;
  let ambient_mute_frame_counter =
    scene.ambient_mute_frame_counter
    - if scene.ambient_mute_frame_counter > 0 then 1 else 0
  in
  if ambient_mute_frame_counter <= mute_fade_in_frames then
    set_music_volume scene.ambient_music
      (mute_volume
      +. (1.0 -. mute_volume)
         *. (float (mute_fade_in_frames - ambient_mute_frame_counter)
            /. float mute_fade_in_frames));
  if ambient_mute_frame_counter == 0 then
    set_music_volume scene.ambient_music 1.0;
  let start_anim = UIAnim.update scene.start_anim in
  let lighting =
    LightingSystem.set_light_position flashlight_tag (Player.position player)
      scene.lighting
  in
  {
    scene with
    player;
    sky;
    start_anim;
    lighting;
    ambient_mute_frame_counter;
    can_interact_ui = false;
  }

let get_screen_to_world_ray (scene : t) =
  let camera = Player.get_view scene.player in
  let cam_pos = Camera3D.position camera in
  let cam_fwd = FPCamera.forward_norm @@ Player.fpcamera scene.player in
  Ray.create cam_pos cam_fwd

let interacted bbox common =
  let look_ray = get_screen_to_world_ray common in
  let col = get_ray_collision_box look_ray bbox in
  let can_interact_with_current =
    RayCollision.hit col && RayCollision.distance col < interaction_distance
  in
  common.can_interact_ui <- can_interact_with_current || common.can_interact_ui;
  is_mouse_button_pressed MouseButton.Left && can_interact_with_current

let player common = common.player
