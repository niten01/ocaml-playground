open Raylib
open Ocaml_playground

module Impl : SceneSign.S = struct
  type t = {
    common : SceneCommons.t;
    picture_tex : Texture2D.t;
    picture_pos : Vector3.t;
    hand_anim : UIAnim.t;
    hand_anim_rect : Rectangle.t;
  }

  let picture_size = 1.

  let get_picture_bbox picture_tex picture_pos =
    let w = float (Texture2D.width picture_tex) *. picture_size in
    let h = float (Texture2D.height picture_tex) *. picture_size in
    let aspect = h /. w in
    let off_h = picture_size *. aspect in
    let off_w = picture_size /. 2. in
    let min =
      Vector3.subtract picture_pos (Vector3.create off_w (off_h /. 2.) off_w)
    in
    let max =
      Vector3.add picture_pos (Vector3.create off_w (off_h /. 2.) off_w)
    in
    BoundingBox.create min max

  let load () =
    let player =
      Player.create "resources/audio/sounds/steps_marble"
        (Vector3.create (-5.) 0. 0.)
        LookDirection.XPlus
    in
    let pillars =
      List.init 20 (fun i ->
          [
            Object.create "resources/models/doric_pillar.glb"
              (Vector3.create (float i *. 5.) 0. 5.)
            |> Object.set_transform (Utils.xzy_to_xyz_transform 0.1);
            Object.create "resources/models/doric_pillar.glb"
              (Vector3.create (float i *. 5.) 0. (-5.))
            |> Object.set_transform (Utils.xzy_to_xyz_transform 0.1);
          ])
      |> List.flatten
    in
    let floor =
      Object.create_no_collision "resources/models/marble_floor.glb"
        (Vector3.zero ())
      |> Object.set_transform @@ Matrix.scale 200. 200. 200.
      |> Object.set_position (Vector3.create 0. 0. 0.)
    in
    let picture_tex = load_texture "resources/textures/picture1.png" in
    let picture_pos = Vector3.create 42.5 2. 5. in
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 1. (-1.) (-1.))
           1.0 Color.white
      |> LightingSystem.add_point_light picture_pos 2.5
           (Color.create 250 220 200 255)
    in
    let objects = pillars @ [ floor ] in
    let hand_anim = UIAnim.create "resources/textures/hand_anim1.png" 128 8 in
    let hand_anim_size = get_screen_width () / 2 in
    let hand_anim_rect =
      Rectangle.create
        (float @@ (get_screen_width () - hand_anim_size))
        (float @@ (get_screen_height () - hand_anim_size))
        (float hand_anim_size) (float hand_anim_size)
    in
    let postprocess_shader_path = "resources/shaders/postprocess_main.fs" in
    let ambient_music_path =
      "resources/audio/music/Ekkehard Ehlers - Round_Rund.mp3"
    in
    {
      common =
        SceneCommons.create objects lighting player postprocess_shader_path
          ambient_music_path;
      picture_tex;
      picture_pos;
      hand_anim;
      hand_anim_rect;
    }

  let unload (scene : t) =
    SceneCommons.destroy scene.common;
    UIAnim.destroy scene.hand_anim;
    unload_texture scene.picture_tex

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.raywhite;
        DebugUtils.draw_axis ();
        SceneCommons.draw scene.common;
        let camera = Player.get_view scene.common.player in
        draw_billboard camera scene.picture_tex scene.picture_pos 1. Color.white)
      scene.common;

    SceneCommons.render_to_screen
      (fun () ->
        draw_fps 10 (get_screen_height () - 20);
        UIAnim.draw scene.hand_anim_rect scene.hand_anim)
      scene.common

  let update scene =
    let common = SceneCommons.update scene.common in
    let picture_pos_y = (sin @@ (get_time () *. 2.0)) *. 0.004 in
    Vector3.set_y scene.picture_pos
      (Vector3.y scene.picture_pos +. picture_pos_y);

    let picture_bbox = get_picture_bbox scene.picture_tex scene.picture_pos in
    match UIAnim.finished scene.hand_anim with
    | true -> ({ scene with common }, Some SceneEnumerator.GraveyardScene)
    | false ->
        let interacted = SceneCommons.interacted picture_bbox common in
        let hand_anim =
          if interacted then scene.hand_anim |> UIAnim.restart |> UIAnim.start
          else scene.hand_anim
        in
        let hand_anim = UIAnim.update hand_anim in

        ({ scene with common; hand_anim = UIAnim.update hand_anim }, None)
end

include Impl
