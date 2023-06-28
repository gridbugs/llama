type t

val create : ?downsample:int -> unit -> t
val set_signal : t -> float Signal.t -> unit
val run : t -> unit Lwt.t
