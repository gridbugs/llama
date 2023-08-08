include module type of struct
  include Llama_core
end

module Live : sig
  val go : unit -> float Signal.t ref

  include module type of struct
    include Dsl
  end
end

val play_signal :
  ?downsample:int -> ?scale_output_volume:float -> float Signal.t -> 'a
(** Play a given signal and never return. The optional [downsample] argument
    artificially reduces the sample rate (the sample rate is divided by this
    value) and is intended for use on slower computers which can't compute
    samples fast enough to keep up with the audio hardware's sample rate. *)

val play_signal_lwt :
  ?downsample:int -> ?scale_output_volume:float -> float Signal.t -> 'a Lwt.t
(** Like [play_signal] but returns a lwt that must be scheduled in order to
    run. Use this when visualizing a signal with [Llama_graphical]. *)

module Dsl = Dsl
module Signal_player = Signal_player
module Signal = Signal
module Float = Float
module Music = Music

module Midi : sig
  include module type of struct
    include Midi
  end

  module Midi_input : sig
    type t
    (** Represents a midi input device *)

    val create : unit -> t
    val port_names : t -> string list
  end

  val live_midi_signal :
    Midi_input.t -> int -> (Event.t list Signal.t, [ `No_such_port ]) result
  (** Create a signal of lists of midi events that arrive on a given midi port in real time *)

  val live_midi_signal_messages :
    Midi_input.t -> int -> (Message.t list Signal.t, [ `No_such_port ]) result
  (** Create a signal of lists of midi events that arrive on a given midi port in real time *)

  val live_midi_sequencer :
    Midi_input.t ->
    port:int ->
    channel:int ->
    polyphony:int ->
    (Midi_sequencer.output, [ `No_such_port ]) result
  (** Create a midi sequencer that processes midi events on a single midi
      channel from a given port. Use this for simple cases where you're only
      interested in events on a single channel. *)

  val live_midi_messages_serial :
    port:string -> baud:int -> Message.t list Signal.t

    val dummy_midi_messages : Message.t list Signal.t
end
