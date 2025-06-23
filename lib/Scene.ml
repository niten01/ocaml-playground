module type S = sig
  type id
  type t

  val load : unit  -> t
  val unload : t -> unit
  val draw : t -> unit
  val update : t  -> t * id option
end

type t = (module S)
