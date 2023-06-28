type t

val create : ?downsample:int -> unit -> t
val signal_ref : t -> float Signal.t ref
val run : t -> unit Lwt.t
