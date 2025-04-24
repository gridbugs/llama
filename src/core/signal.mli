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
val of_signal_ref : 'a t ref -> 'a t
val sample : 'a t -> Ctx.t -> 'a
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
val both : 'a t -> 'b t -> ('a * 'b) t
val force : 'a t -> to_force:_ t -> 'a t
val const : 'a -> 'a t
val var : 'a -> 'a t * 'a ref
val silence : float t
val never : bool t
val scale : float -> float t -> float t
val scale_div : float -> float t -> float t
val offset : float -> float t -> float t

val exp_01 : float -> float t -> float t
(** The function f(x) = exp(k * (x - a)) - b ...where a and b are chosen so that
    f(0) = 0 and f(1) = 1. The k parameter controls how sharp the curve is. It
    approaches a linear function as k approaches 0. k = 0 is special cased as a
    linear function for convenience. *)

val debug : 'a t -> f:('a -> unit) -> 'a t
val debug_print_float_endline : float t -> float t
val debug_print_sample_index_on_true : bool t -> bool t

val to_01 : float t -> float t
(** Takes a signal assumed to be in the range -1..1 and shifts and scales it to
    be in the range 0..1 *)

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

module Trigger : sig
  type 'a signal := 'a t

  type t
  (** A boolean signal which is usually false but becomes true for a single
      sample in response to some event (e.g. a key press) *)

  val rising_edge : ?init:bool -> bool signal -> t
  (** Gate to trigger conversion. Takes a boolean signal and returns a boolean
      signal which is true for only the first sample that its input transitioned
      from false to true. The [init] argument is false by default and helps with
      constructing triggers from other types of signal. If [init] is true and
      the the input signal starts as true then the output signal won't trigger
      until the input becomes false and then true again.*)

  val of_signal_unsafe : bool signal -> t
  (** Takes a signal that is already in the format of a trigger (individual true
      values in mostly false values) and convert it into a trigger. Use this
      when manually constructing a trigger signal and you're sure it has the
      properties of a trigger signal (and isn't, say, a gate signal instead). *)

  val to_signal : t -> bool signal
  val sample : t -> Ctx.t -> bool
  val never : t
  val debug_print_sample_index_on_true : t -> t
end

module Gate : sig
  type 'a signal := 'a t

  type t
  (** A boolean signal which is usually false but becomes true while a certain
      condition is satisfied (e.g. a key is held down) *)

  val to_signal : t -> bool signal
  val of_signal : bool signal -> t
  val to_trigger : t -> Trigger.t
  val sample : t -> Ctx.t -> bool
  val debug_print_sample_index_on_true : t -> t
end

val gate : bool t -> Gate.t
