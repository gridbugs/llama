module Live : sig
  val go : unit -> float Signal.t ref

  include module type of struct
    include Dsl
  end
end

val play_signal : ?downsample:int -> float Signal.t -> 'a
(** Play a given signal and never return. The optional [downsample] argument
    artificially reduces the sample rate (the sample rate is divided by this
    value) and is intended for use on slower computers which can't compute
    samples fast enough to keep up with the audio hardware's sample rate. *)

val play_signal_lwt : ?downsample:int -> float Signal.t -> 'a Lwt.t
(** Like [play_signal] but returns a lwt that must be scheduled in order to
    run. Use this when visualizing a signal with [Llama_graphical]. *)

module Dsl = Dsl
module Signal_player = Signal_player
module Signal = Signal
module Float = Float
module Music = Music
module List = List
module Array = Array
