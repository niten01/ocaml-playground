open Raylib

let std mult = Object.set_transform @@ Utils.xzy_to_xyz_transform mult

let rot_y ang =
  Object.apply_transform @@ Matrix.rotate_y @@ Utils.deg_to_rad ang

let rot_x ang =
  Object.apply_transform @@ Matrix.rotate_x @@ Utils.deg_to_rad ang

let rot_z ang =
  Object.apply_transform @@ Matrix.rotate_z @@ Utils.deg_to_rad ang

let std_rot_y mult ang = fun o -> o |> std mult |> rot_y ang

let scale fact =
 fun o -> o |> Object.apply_transform @@ Matrix.scale fact fact fact

let scale_rot_y mult ang = fun o -> o |> scale mult |> rot_y ang

let move_up dist = 
  fun o -> o|> Object.apply_transform @@ Matrix.translate 0. dist 0.

let rot_y_move_up ang dist =
 fun o ->
  o |> rot_y ang |> move_up dist

let std_rot_move_up mult ang dist =
 fun o -> o |> std mult |> rot_y_move_up ang dist

let scale_rot_y_move_up mult ang dist =
 fun o -> o |> scale_rot_y mult ang |> rot_y_move_up 0. dist

let scale_rot_z_move_up scale ang dist =
 fun o ->
  o
  |> Object.apply_transform @@ Matrix.rotate_z @@ Utils.deg_to_rad ang
  |> scale_rot_y_move_up scale 0. dist

let rot_xyz x y z = fun o -> o |> rot_x x |> rot_y y |> rot_z z
