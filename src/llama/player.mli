module Signal = Llama_core.Signal

module Viz_queue : sig
  type 'a t

  val create : unit -> 'a t
  val drain : 'a t -> f:('a -> unit) -> unit
end

type t

val create : unit -> t
val close : t -> unit

module Viz : sig
  type t = Active
end

module Playing : sig
  type 'a t

  val wait : _ t -> unit
  val viz_queue : 'a t -> 'a Viz_queue.t
end

val play_mono :
  t ->
  ?viz_queue:[ `Use of float Viz_queue.t | `Create ] ->
  ?buffer_size_in_samples:int ->
  ?viz:Viz.t option ->
  float Signal.t ->
  float Playing.t
