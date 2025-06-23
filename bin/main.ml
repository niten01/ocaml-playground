open Ocaml_playground
open Scenes
open Raylib

let width = 1200
let height = 800

module State = struct
  type t = {
    scene_module : SceneSign.t;
    scene_data : bytes;
    scene_name_font : Font.t;
  }
end

let draw_loading_screen font scene_name =
  begin_drawing ();
  clear_background Color.black;
  let scene_name_size = 30. in
  let scene_name_spacing = 2. in
  let scene_name_padding = 20. in
  let text_size =
    measure_text_ex font scene_name scene_name_size scene_name_spacing
  in
  draw_text_pro font scene_name
    (Vector2.create
       (scene_name_padding +. Vector2.y text_size)
       ((float @@ get_screen_height ())
       -. Vector2.x text_size -. scene_name_padding))
    (Vector2.zero ()) 90. scene_name_size scene_name_spacing Color.white;
  end_drawing ()

let setup () =
  set_config_flags [ ConfigFlags.Msaa_4x_hint ];
  init_window width height "playground";
  init_audio_device ();
  SceneCommons.init ();
  set_target_fps 60;
  disable_cursor ();
  FPCamera.set_sensitivity 0.5;
  let scene_name_font = load_font "resources/fonts/Times New Roman.ttf" in
  let (module StartSceneHandler), start_scene_name =
    SceneConverter.get_named_handler SceneEnumerator.MainScene
  in
  draw_loading_screen scene_name_font start_scene_name;
  let scene_data = StartSceneHandler.load () in
  let state =
    State.
      {
        scene_module = (module StartSceneHandler);
        scene_data = Obj.magic scene_data;
        scene_name_font;
      }
  in
  state

let rec loop (state : State.t) =
  match Raylib.window_should_close () with
  | true -> Raylib.close_window ()
  | false ->
      let (module SceneHandler) = state.scene_module in
      let scene_data, new_scene_opt =
        SceneHandler.update @@ Obj.magic state.scene_data
      in
      SceneHandler.draw scene_data;
      let scene_module, scene_data =
        match new_scene_opt with
        | None -> (state.scene_module, scene_data)
        | Some new_scene_id ->
            let (module Handler), scene_name =
              SceneConverter.get_named_handler new_scene_id
            in
            draw_loading_screen state.scene_name_font scene_name;
            SceneHandler.unload scene_data;
            let scene_data = Handler.load () in
            ((module Handler), Obj.magic scene_data)
      in
      loop { state with scene_module; scene_data = Obj.magic scene_data }

let () = setup () |> loop
