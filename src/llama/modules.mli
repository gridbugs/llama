module Oscillator : sig
  type waveform = Sine | Saw | Triangle | Square | Noise

  type t = {
    waveform : waveform Signal.t;
    frequency_hz : float Signal.t;
    square_wave_pulse_width_01 : float Signal.t;
    reset_trigger : bool Signal.t;
    reset_offset_01 : float Signal.t;
  }

  val signal : t -> float Signal.t
end

module Clock : sig
  type t = { frequency_hz : float Signal.t }

  val signal : t -> bool Signal.t
end

module Amplifier : sig
  type t = { signal : float Signal.t; volume : float Signal.t }

  val signal : t -> float Signal.t
end

module Asr_linear : sig
  type t = {
    gate : bool Signal.t;
    attack_s : float Signal.t;
    release_s : float Signal.t;
  }

  val signal : t -> float Signal.t
end

module Adsr_linear : sig
  type t = {
    gate : bool Signal.t;
    attack_s : float Signal.t;
    decay_s : float Signal.t;
    sustain_01 : float Signal.t;
    release_s : float Signal.t;
  }

  val signal : t -> float Signal.t
end

module Sequencer : sig
  type output = { value : float Signal.t; gate : bool Signal.t }
end

module Step_sequencer : sig
  type step = { value : float Signal.t; period_s : float Signal.t }
  type t = { sequence : step option list; clock : bool Signal.t }

  val signal : t -> Sequencer.output
end

module Random_sequencer : sig
  type t = {
    values : float Signal.t list;
    period : float Signal.t;
    clock : bool Signal.t;
  }

  val signal : t -> Sequencer.output
end

module Butterworth_filter : sig
  type t = { signal : float Signal.t; half_power_frequency_hz : float Signal.t }

  val signal_low_pass : t -> filter_order_half:int -> float Signal.t
  val signal_high_pass : t -> filter_order_half:int -> float Signal.t
end

module Chebyshev_filter : sig
  type t = {
    signal : float Signal.t;
    cutoff_hz : float Signal.t;
    epsilon : float Signal.t;
  }

  val signal_low_pass : t -> filter_order_half:int -> float Signal.t
  val signal_high_pass : t -> filter_order_half:int -> float Signal.t
end

module Sample_and_hold : sig
  type t = { signal : float Signal.t; trigger : bool Signal.t }

  val signal : t -> float Signal.t
end