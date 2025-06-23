open Raylib

let sleep_frames = 2

type t = {
  text : string list;
  sounds : Sound.t option list;
  part_counter : int;
  char_counter : int;
  started : bool;
  font_handle : Font.t;
  sleep_frames_counter : int;
  idle_counter : int;
  finished : bool;
}

let create_pro (text_sounds : (string * string option) list) font_handle =
  {
    text = List.map fst text_sounds;
    sounds =
      List.map
        (fun p ->
          match snd p with None -> None | Some file -> Some (load_sound file))
        text_sounds;
    part_counter = 0;
    char_counter = 0;
    started = false;
    font_handle;
    sleep_frames_counter = 0;
    idle_counter = 0;
    finished = false;
  }

let destroy this =
  List.iter
    (fun s -> if Option.is_some s then unload_sound @@ Option.get s)
    this.sounds

let create text_parts font_handle =
  create_pro (List.map (fun t -> (t, None)) text_parts) font_handle

let check_play_sound this part_counter =
  if part_counter >= List.length this.sounds then ()
  else
    match List.nth this.sounds part_counter with
    | None -> ()
    | Some sound -> play_sound sound

let start this =
  check_play_sound this 0;
  {
    this with
    started = true;
    part_counter = 0;
    char_counter = 0;
    sleep_frames_counter = 0;
    idle_counter = 0;
    finished = false;
  }

let finished this = this.finished

let draw this =
  match this.started with
  | false -> ()
  | true ->
      let substring =
        String.sub (List.nth this.text this.part_counter) 0 this.char_counter
        ^
        if this.idle_counter != 0 && this.idle_counter mod 20 < 10 then "   >"
        else ""
      in
      draw_text_ex this.font_handle substring
        (Vector2.create
           ((float @@ get_screen_width ()) *. 0.2)
           ((float @@ get_screen_height ()) *. 0.76))
        20. 1. Color.white

let update this =
  match this.started with
  | false -> this
  | true ->
      let cur_part_len = String.length (List.nth this.text this.part_counter) in
      let key_pressed = get_key_pressed () in
      if this.sleep_frames_counter < sleep_frames && key_pressed == Key.of_int 0
      then { this with sleep_frames_counter = this.sleep_frames_counter + 1 }
      else
        let part_counter, char_counter =
          if this.char_counter == cur_part_len && is_key_pressed Key.Space then (
            check_play_sound this (this.part_counter + 1);
            (this.part_counter + 1, 0))
          else if is_key_pressed Key.Space then (this.part_counter, cur_part_len)
          else if this.char_counter < cur_part_len then
            (this.part_counter, this.char_counter + 1)
          else (this.part_counter, this.char_counter)
        in
        let idle_counter =
          if char_counter == cur_part_len then this.idle_counter + 1
          else if char_counter == 0 then 0
          else this.idle_counter
        in
        let started = part_counter < List.length this.text in
        let finished = not started in
        {
          this with
          char_counter;
          part_counter;
          started;
          idle_counter;
          sleep_frames_counter = 0;
          finished;
        }
