type t

val create : ?downsample:int -> ?initial_signal:float Signal.t -> unit -> t
val signal_ref : t -> float Signal.t ref
val run : t -> unit Lwt.t
