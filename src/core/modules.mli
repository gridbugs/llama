module Oscillator : sig
  type waveform = Sine | Saw | Triangle | Pulse | Noise

  val waveform_to_string : waveform -> string

  type t = {
    waveform : waveform Signal.t;
    frequency_hz : float Signal.t;
    pulse_width_01 : float Signal.t;
    reset_trigger : Signal.Trigger.t;
    reset_offset_01 : float Signal.t;
  }

  val signal : t -> float Signal.t
end

(** A trigger signal that is true at regular intervals *)
module Clock : sig
  type t = { frequency_hz : float Signal.t }

  val trigger : t -> Signal.Trigger.t
end

module Clock_divider : sig
  type t = { clock : Signal.Trigger.t; denominator : int }

  val trigger : t -> Signal.Trigger.t
end

module Ar_linear : sig
  type t = {
    gate : Signal.Gate.t;
    attack_s : float Signal.t;
    release_s : float Signal.t;
  }

  val signal : t -> float Signal.t
end

module Adsr_linear : sig
  type t = {
    gate : Signal.Gate.t;
    attack_s : float Signal.t;
    decay_s : float Signal.t;
    sustain_01 : float Signal.t;
    release_s : float Signal.t;
  }

  val signal : t -> float Signal.t
end

module Sequencer : sig
  type 'a output = { value : 'a Signal.t; gate : Signal.Gate.t }
  type 'a step = { value : 'a Signal.t; period_s : float Signal.t }
end

module Sustained_step_sequencer : sig
  type step = float Sequencer.step
  type t = { sequence : step option list; clock : Signal.Trigger.t }

  val signal : t -> float Sequencer.output
end

module Generic_step_sequencer : sig
  type 'a step = 'a Sequencer.step
  type 'a t = { sequence : 'a step list; clock : Signal.Trigger.t }

  val signal : 'a t -> 'a Sequencer.output
end

module Random_sequencer : sig
  type 'a t = {
    values : 'a Signal.t list;
    period : float Signal.t;
    clock : Signal.Trigger.t;
  }

  val signal : 'a t -> 'a Sequencer.output
end

module Butterworth_filter : sig
  type t = { signal : float Signal.t; cutoff_hz : float Signal.t }
  (** The literature refers to the cutoff frequency as the "half power frequency" *)

  val signal_low_pass : t -> filter_order_half:int -> float Signal.t
  val signal_high_pass : t -> filter_order_half:int -> float Signal.t
end

module Chebyshev_filter : sig
  type t = {
    signal : float Signal.t;
    cutoff_hz : float Signal.t;
    resonance : float Signal.t;
  }
  (** Note that in the literature this filter has a parameter called "epsilon".
      The resonance field of this type corresponds to epsilon. *)

  val signal_low_pass : t -> filter_order_half:int -> float Signal.t
  val signal_high_pass : t -> filter_order_half:int -> float Signal.t
end

module Sample_and_hold : sig
  type t = { signal : float Signal.t; trigger : Signal.Trigger.t }

  val signal : t -> float Signal.t
end

module Sample_player_mono : sig
  type t = { data : floatarray; trigger : Signal.Trigger.t }

  val signal : t -> float Signal.t
end

module Bitwise_trigger_sequencer : sig
  type t = {
    num_channels : int;
    sequence : int Signal.t list;
    clock : Signal.Trigger.t;
  }

  val triggers : t -> Signal.Trigger.t list
end

module Delay : sig
  type 'a t = { signal : 'a Signal.t; time_s : float Signal.t; fill : 'a }

  val signal : 'a t -> 'a Signal.t
end

module Lazy_amplifier : sig
  type t = { signal : float Signal.t; volume : float Signal.t }

  val signal : t -> float Signal.t
end
