module Ctx : sig
  type t = { sample_index : int; sample_rate_hz : float }
end

module Raw : sig
  type 'a t = Ctx.t -> 'a

  val with_state : init:'a -> f:('a -> 'a t) -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end

type 'a t

val of_raw : 'a Raw.t -> 'a t
val of_ref : 'a ref -> 'a t
val sample : 'a t -> Ctx.t -> 'a
val map : 'a t -> f:('a -> 'b) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val const : 'a -> 'a t
val var : 'a -> 'a t * 'a ref
val trigger : bool t -> bool t
