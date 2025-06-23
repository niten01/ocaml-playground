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
    statue_final_text : UITextAnim.t;
    angel_serenity_obj : Object.t;
    angel_sorrow_obj : Object.t;
    angel_loneliness_obj : Object.t;
    angel_obedience_obj : Object.t;
    angel_serenity_text : UITextAnim.t;
    angel_sorrow_text : UITextAnim.t;
    angel_loneliness_text : UITextAnim.t;
    angel_obedience_text : UITextAnim.t;
    returned_memories : Utils.StringSet.t;
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
    let graveyard = gen_graves [] 0 in
    let statue_obj =
      Object.create "resources/models/graveyard/statue_silence.glb"
        (Vector3.create 1. 10. 0.)
      |> scale_rot_y 20. (-90.)
      |> Object.set_bbox
         @@ BoundingBox.create
              (Vector3.create (-3.) 0. (-3.))
              (Vector3.create 3. 10. 3.)
    in
    let angel_serenity_obj =
      place_obj "angel_statue1" (Vector3.create 50. 0. 43.)
      |> rot_y (-40.)
      |> Object.set_bbox
           (BoundingBox.create
              (Vector3.create 52. 0. 40.)
              (Vector3.create 48. 5. 45.))
      |> Object.set_no_collision false
    in
    let angel_sorrow_obj =
      place_obj "angel_statue2" (Vector3.create (-50.) 0. 53.)
      |> rot_y (-90.)
      |> Object.set_bbox
           (BoundingBox.create
              (Vector3.create (-48.) 0. 51.)
              (Vector3.create (-52.) 5. 55.))
      |> Object.set_no_collision false
    in
    let angel_loneliness_obj =
      place_obj "angel_statue3" (Vector3.create (-50.) 0. (-50.))
      |> rot_y 120.
      |> Object.set_no_collision false
    in
    let angel_obedience_obj =
      place_obj "angel_statue4" (Vector3.create 55. 0. (-40.))
      |> rot_y 40.
      |> Object.set_no_collision false
    in
    let objects =
      [
        statue_obj;
        floor;
        angel_serenity_obj;
        angel_sorrow_obj;
        angel_loneliness_obj;
        angel_obedience_obj;
      ]
      @ graveyard
    in
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
      |> LightingSystem.add_point_light
           (Vector3.create 48. 5. 42.)
           2. Color.white
      |> LightingSystem.add_point_light
           (Vector3.create (-48.) 5. 51.)
           1.5 Color.skyblue
      |> LightingSystem.add_point_light
           (Vector3.create 54. 5. (-39.))
           1.5 Color.darkbrown
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
      "resources/audio/music/Joseph Suchy - Soan-Ne (remastered).mp3"
    in
    let common =
      SceneCommons.create objects lighting player postprocess_shader_path
        ambient_music_path
    in
    let statue_text =
      UITextAnim.create
        [
          "Despite its rather formidable appearance, this statue exudes a \
           sense of righteousness...";
          "It feels as though it longs remembrance.";
        ]
        (SceneCommons.get_text_font ())
    in
    let statue_final_text =
      UITextAnim.create
        [ "Statue looks less turmoiled now..." ]
        (SceneCommons.get_text_font ())
    in
    let angel_serenity_text =
      UITextAnim.create_pro
        [
          ( "A wave of serenity washes over the surroundings as the gaze meets \
             this angel...",
            Some "resources/audio/sounds/serenity_fx.mp3" );
          ( "It is as if a fragment of a tranquil soul, once lost, has quietly \
             returned.",
            None );
        ]
        (SceneCommons.get_text_font ())
    in
    let angel_sorrow_text =
      UITextAnim.create_pro
        [
          ( "A quiet sorrow, steeped in nostalgia, begins to settle in the mind.",
            Some "resources/audio/sounds/sorrow_fx.mp3" );
          ( "This angel evokes echoes of a lost youth, of places that never \
             were, and the\n\n\
             hollow resonance of a distant life...",
            None );
          ( "The vision slowly fades - yet something lingers, peacefully left \
             behind.",
            None );
        ]
        (SceneCommons.get_text_font ())
    in
    let angel_loneliness_text =
      UITextAnim.create_pro
        [
          ( "This angel's face clears the mind of all but a singular feeling - \
             loneliness...",
            Some "resources/audio/sounds/loneliness_fx.mp3" );
          ( "Its submissive gaze stirs a primal unease: the ancient dread of \
             eternal solitude.",
            None );
          ( "And yet, the feeling is not burdensome; in its posture lies a \
             calm, impartial\n\n\
             acceptance of this inescapable thought.",
            None );
        ]
        (SceneCommons.get_text_font ())
    in
    let angel_obedience_text =
      UITextAnim.create_pro
        [
          ( "Beneath the stone veil emerges an obedient face, shaped by the \
             rejection of its true self.",
            Some "resources/audio/sounds/obedience_fx.mp3" );
          ( "This angel's expression shifts - from a disquieting sense of \
             futility to an embodiment\n\n\
             of pure independence.",
            None );
        ]
        (SceneCommons.get_text_font ())
    in
    {
      common;
      sky;
      statue_obj;
      flying_stuff;
      statue_text;
      statue_final_text;
      angel_serenity_obj;
      angel_sorrow_obj;
      angel_loneliness_obj;
      angel_obedience_obj;
      angel_serenity_text;
      angel_sorrow_text;
      angel_loneliness_text;
      angel_obedience_text;
      returned_memories = Utils.StringSet.empty;
    }

  let unload (scene : t) =
    SceneCommons.destroy scene.common;
    Object.destroy scene.sky;
    List.iter Object.destroy scene.flying_stuff;
    UITextAnim.destroy scene.statue_text;
    UITextAnim.destroy scene.statue_final_text;
    UITextAnim.destroy scene.angel_serenity_text;
    UITextAnim.destroy scene.angel_sorrow_text;
    UITextAnim.destroy scene.angel_loneliness_text;
    UITextAnim.destroy scene.angel_obedience_text

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
        UITextAnim.draw scene.statue_text;
        UITextAnim.draw scene.statue_final_text;
        UITextAnim.draw scene.angel_serenity_text;
        UITextAnim.draw scene.angel_sorrow_text;
        UITextAnim.draw scene.angel_loneliness_text;
        UITextAnim.draw scene.angel_obedience_text)
      scene.common

  let angel_interaction angel_obj angel_text common =
    let interacted = SceneCommons.interacted (Object.bbox angel_obj) common in
    let angel_text =
      angel_text
      |> (if interacted then UITextAnim.start else Fun.id)
      |> UITextAnim.update
    in
    (angel_text, interacted)

  let update scene =
    let common = scene.common |> SceneCommons.update in
    let sky = rotate_sky scene.sky in
    let flying_stuff = randomize_flying_stuff scene.flying_stuff in
    let interacted =
      SceneCommons.interacted (Object.bbox scene.statue_obj) common
    in
    let all_memories_returned =
      List.length @@ Utils.StringSet.elements scene.returned_memories == 4
    in
    let statue_text =
      scene.statue_text
      |> (if interacted && not all_memories_returned then UITextAnim.start
          else Fun.id)
      |> UITextAnim.update
    in
    let statue_final_text =
      scene.statue_final_text
      |> (if interacted && all_memories_returned then UITextAnim.start
          else Fun.id)
      |> UITextAnim.update
    in
    if UITextAnim.finished statue_final_text then
      (scene, Some SceneEnumerator.MainScene)
    else
      let returned_memories = scene.returned_memories in
      let angel_serenity_text, interacted =
        angel_interaction scene.angel_serenity_obj scene.angel_serenity_text
          common
      in
      let common =
        if interacted then SceneCommons.mute_for 20 common else common
      in
      let returned_memories =
        returned_memories
        |> if interacted then Utils.StringSet.add "serenity" else Fun.id
      in

      let angel_sorrow_text, interacted =
        angel_interaction scene.angel_sorrow_obj scene.angel_sorrow_text common
      in
      let common =
        if interacted then SceneCommons.mute_for 25 common else common
      in
      let returned_memories =
        returned_memories
        |> if interacted then Utils.StringSet.add "sorrow" else Fun.id
      in

      let angel_loneliness_text, interacted =
        angel_interaction scene.angel_loneliness_obj scene.angel_loneliness_text
          common
      in
      let common =
        if interacted then SceneCommons.mute_for 19 common else common
      in
      let returned_memories =
        returned_memories
        |> if interacted then Utils.StringSet.add "loneliness" else Fun.id
      in

      let angel_obedience_text, interacted =
        angel_interaction scene.angel_obedience_obj scene.angel_obedience_text
          common
      in
      let common =
        if interacted then SceneCommons.mute_for 21 common else common
      in
      let returned_memories =
        returned_memories
        |> if interacted then Utils.StringSet.add "obedience" else Fun.id
      in
      ( {
          scene with
          common;
          sky;
          flying_stuff;
          statue_text;
          statue_final_text;
          angel_serenity_text;
          angel_sorrow_text;
          angel_loneliness_text;
          angel_obedience_text;
          returned_memories;
        },
        None )
end

include Impl
