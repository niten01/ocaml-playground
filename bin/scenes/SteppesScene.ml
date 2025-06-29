open Raylib
open Ocaml_playground
open Transforms

module Impl : SceneSign.S = struct
  type t = { common : SceneCommons.t }

  let load () =
    let player =
      Player.create "resources/audio/sounds/steps_barefeet"
        (Vector3.create (-60.) 0. 0.)
        LookDirection.XPlus
    in
    let sky =
      Object.create_pro "resources/models/steppes/sky.glb" (Vector3.zero ())
        true true
      |> scale 150.
    in
    let ground_obj =
      Object.create_no_collision "resources/models/steppes/ground.glb"
        (Vector3.zero ())
      |> scale 1000.
    in
    let foliage =
      let base_path = "resources/models/steppes/foliage/" in
      let paths = Sys.readdir base_path |> Array.map (fun p -> base_path ^ p) in
      List.init 7000 (fun _ ->
          let random_index = Random.int @@ Array.length paths in
          let random_path = paths.(random_index) in
          let random_rot = Utils.random_float 0. 360. in
          let area = 100. in
          Object.create_no_collision random_path
            (Utils.random_vec3 (-.area, area) (0., 0.) (-.area, area))
          |> scale_rot_y 10. random_rot)
    in
    let objects = [ ground_obj ] @ foliage in
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 0.3 (-1.) 0.7)
           0.1
           (Color.create 130 80 20 255)
      (* Color.white *)
    in
    let postprocess_shader_path = "resources/shaders/postprocess_steppes.fs" in
    let ambient_music_path =
      "resources/audio/music/Joseph Suchy - Soan-Ne (remastered).mp3"
    in
    let common =
      SceneCommons.create objects lighting player postprocess_shader_path
        ambient_music_path
      |> SceneCommons.set_sky sky
    in
    { common }

  let unload (scene : t) = SceneCommons.destroy scene.common

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.black;
        DebugUtils.draw_axis ();
        SceneCommons.draw scene.common)
      scene.common;

    SceneCommons.render_to_screen
      (fun () -> draw_fps 10 (get_screen_height () - 20))
      scene.common

  let update scene =
    let common = scene.common |> SceneCommons.update in
    ({ common }, None)
end

include Impl
