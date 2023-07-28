include module type of struct
  include Llama_midi
end

module Controller_table : sig
  type t

  val get_raw : t -> int -> float Signal.t
  val modulation : t -> float Signal.t
  val volume : t -> float Signal.t
end

module Gate_table : sig
  type t

  val get : t -> int -> Signal.Gate.t
end

module Midi_sequencer : sig
  type voice = {
    frequency_hz : float Signal.t;
    gate : Signal.Gate.t;
    velocity : int Signal.t;
  }

  type output = {
    voices : voice list;
    pitch_wheel_multiplier : float Signal.t;
    controller_table : Controller_table.t;
  }

  val key_gates : channel:int -> Event.t list Signal.t -> Gate_table.t
  val signal : channel:int -> polyphony:int -> Event.t list Signal.t -> output
end

val track_signal : Track.t -> Signal.Trigger.t -> Event.t list Signal.t
