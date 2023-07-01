type t

val create : ?downsample:int -> ?initial_signal:float Signal.t -> unit -> t
val signal_ref : t -> float Signal.t ref

val run : t -> 'a Lwt.t
(** Run the palyer and never return *)
