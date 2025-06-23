open Raylib

type t = {
  model_source : string;
  model : Model.t;
  position : Vector3.t;
  transform : Matrix.t;
  bbox : BoundingBox.t;
  no_collide : bool;
  no_light : bool;
}

let shift_bbox delta bbox =
  let min = BoundingBox.min bbox in
  let max = BoundingBox.max bbox in
  BoundingBox.create (Vector3.add min delta) (Vector3.add max delta)

let set_bbox bbox obj = { obj with bbox }
let bbox obj = obj.bbox
let set_no_collision value obj = { obj with no_collide = value }

let apply_shader shader obj =
  if obj.no_light then ()
  else
    CArray.iter
      (fun mat -> Material.set_shader mat shader)
      (Model.materials obj.model)

let create_pro path_to_model position no_collide no_light =
  let model = ResourcePool.use path_to_model in
  let bbox = get_model_bounding_box model |> shift_bbox position in
  {
    model_source = path_to_model;
    model;
    position;
    bbox;
    no_collide;
    no_light;
    transform = Matrix.identity ();
  }

let create path_to_model position =
  create_pro path_to_model position false false

let create_no_collision path_to_model position =
  create_pro path_to_model position true false

let destroy obj = ResourcePool.free obj.model_source

let set_transform transform obj =
  Model.set_transform obj.model transform;
  let bbox = get_model_bounding_box obj.model |> shift_bbox obj.position in
  { obj with bbox; transform }

let apply_transform transform obj =
  let new_mat = Matrix.multiply obj.transform transform in
  set_transform new_mat obj

let position obj = obj.position

let set_position position obj =
  let delta = Vector3.subtract position obj.position in
  let bbox = shift_bbox delta obj.bbox in
  { obj with position; bbox }

let collides_with player obj =
  match obj.no_collide with
  | true -> false
  | false -> check_collision_boxes obj.bbox (Player.bbox player)

let draw obj =
  Model.set_transform obj.model obj.transform;
  draw_model obj.model obj.position 1. Color.white
