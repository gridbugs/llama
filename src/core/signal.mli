module Ctx : sig
  type t = { sample_index : int; sample_rate_hz : float }
end

module Raw : sig
  type 'a t = Ctx.t -> 'a

  val with_state : init:'state -> f:('state -> 'state t) -> 'state t
  val with_state' : init:'state -> f:('state -> ('state * 'a) t) -> 'a t
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
val silence : float t
val never : bool t
val trigger : ?init:bool -> bool t -> bool t
val scale : float -> float t -> float t
val scale_div : float -> float t -> float t
val offset : float -> float t -> float t

val exp_01 : float -> float t -> float t
(** The function f(x) = exp(k * (x - a)) - b
   ...where a and b are chosen so that f(0) = 0 and f(1) = 1.
   The k parameter controls how sharp the curve is.
   It approaches a linear function as k approaches 0.
   k = 0 is special cased as a linear function for convenience. *)

val debug : 'a t -> f:('a -> unit) -> 'a t
val debug_print_float_endline : float t -> float t

val to_01 : float t -> float t
(** Takes a signal assumed to be in the range -1..1 and shifts and scales it to be in the range 0..1 *)

val recip : float t -> float t
val sum : float t list -> float t
val mean : float t list -> float t
val add : float t -> float t -> float t
val ( +.. ) : float t -> float t -> float t
val mul : float t -> float t -> float t
val ( *.. ) : float t -> float t -> float t
val sub : float t -> float t -> float t
val ( -.. ) : float t -> float t -> float t
val div : float t -> float t -> float t
val ( /.. ) : float t -> float t -> float t
