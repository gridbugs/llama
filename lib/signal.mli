module Ctx : sig
  type t = { sample_index : int; sample_rate_hz : float }
end

module Raw : sig
  type 'a t = Ctx.t -> 'a
end

type 'a t

module type Ops = sig
  val of_raw : 'a Raw.t -> 'a t
  val of_ref : 'a ref -> 'a t
  val sample : 'a t -> Ctx.t -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val const : 'a -> 'a t
  val var : 'a -> 'a t * 'a ref
end

include Ops
