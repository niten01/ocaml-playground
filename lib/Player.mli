type t

val create : string ->  Raylib.Vector3.t -> LookDirection.t -> t
val destroy : t -> unit
val fpcamera : t -> FPCamera.t
val position : t -> Raylib.Vector3.t
val bbox : t -> Raylib.BoundingBox.t
val get_view : t -> Raylib.Camera3D.t
val draw_2d : t -> unit
val update : t -> t
val undo_movement : t -> t
