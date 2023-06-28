val a : string

module Output_stream : sig
  type t

  val hello : unit -> unit
  val new_player : unit -> t
  val example : t -> float -> int32
end
