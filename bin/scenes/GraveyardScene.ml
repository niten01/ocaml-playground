open Raylib
open Ocaml_playground

let randomize_frame_counter = ref 0

module Impl : SceneSign.S = struct
  type t = {
    common : SceneCommons.t;
    sky : Object.t;
    statue_obj : Object.t;
    flying_stuff : Object.t list;
    statue_text : UITextAnim.t;
  }

  let randomize (obj : Object.t) =
    let x = Utils.random_float (-50.) 50. in
    let y = Utils.random_float 0. 30. in
    let z = Utils.random_float (-50.) 50. in
    let angle = Utils.deg_to_rad @@ Utils.random_float 0. 360. in
    let new_obj =
      obj
      |> Object.set_position (Vector3.create x y z)
      |> Object.apply_transform
           (Matrix.rotate_xyz (Vector3.create angle angle angle))
    in
    new_obj

  let load () =
    let player =
      Player.create "resources/audio/sounds/steps_barefeet"
        (Vector3.create (-60.) 0. 0.)
        LookDirection.XPlus
    in
    let floor =
      Object.create_pro "resources/models/marble_floor_black.glb"
        (Vector3.zero ()) true false
      |> Object.set_transform @@ Matrix.scale 100. 100. 100.
    in
    let g_model_name name = "resources/models/graveyard/" ^ name ^ ".glb" in
    let std scale = Object.set_transform @@ Utils.xzy_to_xyz_transform scale in
    let rot_y ang =
      Object.apply_transform @@ Matrix.rotate_y @@ Utils.deg_to_rad ang
    in
    let std_rot_y scale ang = fun o -> o |> std scale |> rot_y ang in
    let scale_rot_y scale ang =
     fun o ->
      o |> Object.apply_transform @@ Matrix.scale scale scale scale |> rot_y ang
    in
    let rot_move_up ang dist =
     fun o ->
      o |> rot_y ang |> Object.apply_transform @@ Matrix.translate 0. dist 0.
    in
    let std_rot_move_up scale ang dist =
     fun o -> o |> std scale |> rot_move_up ang dist
    in
    let scale_rot_y_move_up scale ang dist =
     fun o -> o |> scale_rot_y scale ang |> rot_move_up 0. dist
    in
    let scale_rot_z_move_up scale ang dist =
     fun o ->
      o
      |> Object.apply_transform @@ Matrix.rotate_z @@ Utils.deg_to_rad ang
      |> scale_rot_y_move_up scale 0. dist
    in
    let graveyard_opts =
      Utils.StringMap.of_seq
      @@ List.to_seq
           [
             ("gravestone1", std 1.5);
             ("gravestone1_alt", std_rot_y 1.5 180.);
             ("gravestone2", scale_rot_y_move_up 0.5 (-120.) 0.7);
             ("gravestone3", scale_rot_y 2. (-90.));
             ("graveplate", std_rot_y 2. 180.);
             ("cross1", std_rot_y 2. 180.);
             ("cross2", rot_move_up (-90.) 0.7);
             ("cross3", rot_move_up (-90.) 0.7);
             ("big_tomb1", std_rot_y 2. 90.);
             ("big_tomb2", scale_rot_y_move_up 2. (-90.) 0.6);
             ("angel_statue1", scale_rot_z_move_up 0.5 180. (-4.));
             ("angel_statue2", std_rot_move_up 2. (-100.) 3.0);
             ("angel_statue3", std_rot_y 0.5 (-90.));
             ("angel_statue4", scale_rot_y 2.5 (-90.));
           ]
    in
    let place_obj name pos =
      let transform = Utils.StringMap.find name graveyard_opts in
      Object.create_no_collision (g_model_name name) pos |> transform
    in
    let randomizable_objects =
      [
        "gravestone1";
        "gravestone1_alt";
        "gravestone2";
        "gravestone3";
        "graveplate";
        "cross1";
        "cross2";
        "cross3";
        "big_tomb1";
        "big_tomb2";
      ]
    in
    Random.init 1350;
    let rec gen_graves acc i =
      if i == 300 then acc
      else
        let random_index = Random.int @@ List.length randomizable_objects in
        let rnd_obj_name = List.nth randomizable_objects random_index in
        let rnd_coord () = Utils.random_float (-50.) 50. in
        let rnd_vec = Vector3.create (rnd_coord ()) 0. (rnd_coord ()) in
        let rnd_angle = Utils.random_float 0. 360. in
        if
          Option.is_some
          @@ List.find_opt
               (fun o -> Vector3.distance (Object.position o) rnd_vec < 3.)
               acc
        then gen_graves acc i
        else
          let new_obj = place_obj rnd_obj_name rnd_vec |> rot_y rnd_angle in
          gen_graves (new_obj :: acc) (i + 1)
    in
    let statue_obj =
      Object.create "resources/models/graveyard/statue_silence.glb"
        (Vector3.create 1. 10. 0.)
      |> scale_rot_y 20. (-90.)
      |> Object.set_bbox
         @@ BoundingBox.create
              (Vector3.create (-3.) 0. (-3.))
              (Vector3.create 3. 10. 3.)
    in
    let graveyard = gen_graves [] 0 in
    let objects = [ statue_obj; floor ] @ graveyard in
    let sky =
      Object.create_pro "resources/models/marble_sphere_black.glb"
        (Vector3.zero ()) true true
      |> Object.set_transform @@ Matrix.scale 300. 300. 300.
      |> Object.set_position (Vector3.create 0. 0. 0.)
    in
    let lighting =
      LightingSystem.create ()
      |> LightingSystem.add_dir_light (Vector3.create 0. 0. 0.)
           (Vector3.create 0.3 (-1.) 0.7)
           0.5
           (Color.create 180 180 230 255)
    in
    let barriers =
      List.init 6 (fun i ->
          Object.create_pro
            ("resources/models/barriers/barrier" ^ string_of_int i ^ ".glb")
            (Vector3.create 0. (-100.) 0.)
            false false
          |> Object.set_transform @@ Matrix.scale 3. 3. 3.
          |> randomize)
    in
    let flying_stuff = barriers in
    List.iter
      (Object.apply_shader @@ LightingSystem.shader lighting)
      flying_stuff;
    let postprocess_shader_path = "resources/shaders/postprocess_main.fs" in
    let ambient_music_path =
      "resources/audio/music/Joseph Suchy - Soan-Ne.mp3"
    in
    let common =
      SceneCommons.create objects lighting player postprocess_shader_path
        ambient_music_path
    in
    let statue_text =
      UITextAnim.create
        [
          "In spite of it's quite formidable appearance this statue looks so \
           righteous...";
          "It feels like it seeks remembrance.";
        ]
        (SceneCommons.get_text_font ())
    in
    { common; sky; statue_obj; flying_stuff; statue_text }

  let unload (scene : t) =
    SceneCommons.destroy scene.common;
    Object.destroy scene.sky;
    List.iter Object.destroy scene.flying_stuff

  let rotate_sky (sky : Object.t) =
    let trig_coeff = 0.0004 in
    let mouse_coeff = 0.001 in
    let mouse_delta = get_mouse_delta () in
    let dx, dy = (Vector2.x mouse_delta, Vector2.y mouse_delta) in
    let t = get_time () in
    let sin, cos = (sin t, cos t) in
    let const_rot = 0.001 in
    let sky =
      Object.apply_transform
        (Matrix.multiply
           (Matrix.rotate_xyz
              (Vector3.create
                 ((trig_coeff *. sin) +. const_rot)
                 ((trig_coeff *. cos) +. const_rot)
                 ((trig_coeff *. sin *. cos) +. const_rot)))
           (Matrix.rotate_xyz
           @@ Vector3.create (dy *. mouse_coeff) (dx *. mouse_coeff)
                ((dx +. dy) /. 2. *. mouse_coeff)))
        sky
    in
    sky

  let randomize_flying_stuff (stuff : Object.t list) =
    let randomize_frame_counter_max = 90 in
    let const_rot = 0.003 in
    randomize_frame_counter := !randomize_frame_counter + 1;
    let do_randomize =
      !randomize_frame_counter >= randomize_frame_counter_max
    in
    if do_randomize then randomize_frame_counter := 0;
    List.map
      (fun obj ->
        if do_randomize then (
          randomize_frame_counter := 0;
          randomize obj)
        else
          obj
          |> Object.apply_transform
               (Matrix.rotate_xyz
               @@ Vector3.create const_rot const_rot const_rot))
      stuff

  let draw (scene : t) =
    SceneCommons.render_to_texture
      (fun () ->
        clear_background Color.black;
        DebugUtils.draw_axis ();
        List.iter Object.draw scene.flying_stuff;
        Object.draw scene.sky;
        SceneCommons.draw scene.common)
      scene.common;

    SceneCommons.render_to_screen
      (fun () ->
        draw_fps 10 (get_screen_height () - 20);
        UITextAnim.draw scene.statue_text)
      scene.common

  let update scene =
    let common = scene.common |> SceneCommons.update in
    let sky = rotate_sky scene.sky in
    let flying_stuff = randomize_flying_stuff scene.flying_stuff in
    let interacted, common =
      SceneCommons.interacted (Object.bbox scene.statue_obj) common
    in
    let statue_text =
      scene.statue_text |> if interacted then UITextAnim.start else Fun.id
    in
    let statue_text = statue_text |> UITextAnim.update in
    ({ scene with common; sky; flying_stuff; statue_text }, None)
end

include Impl
