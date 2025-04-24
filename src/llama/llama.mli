include module type of struct
  include Llama_core
end

module Live : sig
  val go : ?buffer_size_in_samples:int -> unit -> float Signal.t ref

  include module type of struct
    include Dsl
  end
end

val play_signal :
  ?buffer_size_in_samples:int ->
  ?viz:Player.Viz.t option ->
  float Signal.t ->
  unit
(** Play a given signal and never return. *)

module Player = Player
module Dsl = Dsl
module Signal = Signal
module Float = Float
module Music = Music

module Midi : sig
  include module type of struct
    include Midi
  end

  val dummy_midi_messages : Message.t list Signal.t
end
