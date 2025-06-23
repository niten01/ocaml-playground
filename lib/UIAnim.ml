open Raylib

type t = {
  anim_tex : Texture2D.t;
  max_frame : int;
  current_frame : int;
  frame_counter : int;
  frame_width : int;
  slow_scale : int;
  started : bool;
}

let create path_to_anim frame_width slow_scale =
  let anim_tex = load_texture path_to_anim in
  let width = Texture2D.width anim_tex in
  let max_frame = (width / frame_width) - 1 in
  {
    anim_tex;
    max_frame;
    current_frame = 0;
    frame_width;
    slow_scale;
    started = false;
    frame_counter = 1;
  }

let destroy ui_anim = unload_texture ui_anim.anim_tex
let width ui_anim = ui_anim.frame_width
let height ui_anim = Texture2D.height ui_anim.anim_tex
let current_frame ui_anim = ui_anim.current_frame

let draw screen_rect ui_anim =
  draw_texture_pro ui_anim.anim_tex
    (Rectangle.create
       (float @@ (ui_anim.frame_width * ui_anim.current_frame))
       0.
       (float ui_anim.frame_width)
       (float @@ Texture2D.height ui_anim.anim_tex))
    screen_rect (Vector2.zero ()) 0. Color.white

let update ui_anim =
  match ui_anim.started with
  | false -> ui_anim
  | true ->
      if ui_anim.frame_counter >= ui_anim.slow_scale then
        if ui_anim.current_frame = ui_anim.max_frame then ui_anim
        else
          {
            ui_anim with
            current_frame = ui_anim.current_frame + 1;
            frame_counter = 1;
          }
      else { ui_anim with frame_counter = ui_anim.frame_counter + 1 }

let start ui_anim = { ui_anim with started = true }
let started ui_anim = ui_anim.started
let finished ui_anim = ui_anim.current_frame = ui_anim.max_frame
let restart ui_anim = { ui_anim with current_frame = 0 }
