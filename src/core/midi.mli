include module type of struct
  include Llama_midi
end

module Controller_table : sig
  type t

  val get : t -> int -> float Signal.t
end

module Midi_sequencer : sig
  type voice = {
    frequency_hz : float Signal.t;
    gate : bool Signal.t;
    velocity : int Signal.t;
  }

  type output = {
    voices : voice list;
    pitch_wheel_multiplier : float Signal.t;
    controller_table : Controller_table.t;
  }

  val signal : channel:int -> polyphony:int -> Event.t list Signal.t -> output
end

val track_signal : Track.t -> bool Signal.t -> Event.t list Signal.t
