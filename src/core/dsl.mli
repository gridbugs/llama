open! Modules

include module type of struct
  include Signal
end

type waveform = Sine | Saw | Triangle | Pulse | Noise

val waveform_to_string : waveform -> string
val oscillator : ?pulse_width_01:float t -> waveform t -> float t -> float t
val noise_01 : unit -> float t
val noise : min:float t -> max:float t -> float t

val low_frequency_oscillator :
  ?pulse_width_01:float t ->
  ?reset_offset_01:float t ->
  waveform t ->
  float t ->
  bool t ->
  float t

val low_frequency_oscillator_01 :
  ?pulse_width_01:float t ->
  ?reset_offset_01:float t ->
  waveform t ->
  float t ->
  bool t ->
  float t

val clock : float t -> bool t
(** A clock signal with a given frequency in Hz *)

val clock_of_period_s : float t -> bool t
val clock_divide : int -> bool t -> bool t

val ar_linear : gate:bool t -> attack_s:float t -> release_s:float t -> float t
(** Envelope generator with an attack and release parameter. This returns a
    signal which rises linearly to 1 when gate is true, and drops linearly to 0
    when gate is false *)

val adsr_linear :
  gate:bool t ->
  attack_s:float t ->
  decay_s:float t ->
  sustain_01:float t ->
  release_s:float t ->
  float t

type 'a sequencer_output = { value : 'a Signal.t; gate : bool Signal.t }
type 'a sequencer_step = { value : 'a Signal.t; period_s : float Signal.t }

val sustained_step_sequencer :
  float sequencer_step option list -> bool t -> float sequencer_output

val generic_step_sequencer :
  'a sequencer_step list -> bool t -> 'a sequencer_output

val random_sequencer : 'a t list -> float t -> bool t -> 'a sequencer_output
val value_sequencer : 'a t list -> bool t -> 'a t

val butterworth_low_pass_filter :
  ?filter_order_half:int -> float t -> cutoff_hz:float t -> float t

val butterworth_high_pass_filter :
  ?filter_order_half:int -> float t -> cutoff_hz:float t -> float t

val chebyshev_low_pass_filter :
  ?filter_order_half:int ->
  float t ->
  cutoff_hz:float t ->
  resonance:float t ->
  float t

val chebyshev_high_pass_filter :
  ?filter_order_half:int ->
  float t ->
  cutoff_hz:float t ->
  resonance:float t ->
  float t

val sample_and_hold : float t -> bool t -> float t
val sample_player_mono : floatarray -> bool t -> float t
val bitwise_trigger_sequencer : int -> int t list -> bool t -> bool t list
val delay : 'a t -> time_s:float t -> fill:'a -> 'a t
val clock_delay : float -> bool t -> bool t
val periodic_gate : frequency_hz:float t -> duty_01:float t -> bool t

val feedback : f:(float t -> float t) -> float t -> float t
(** [feedback ~f s] adds its previous output modifier by [f] to its input. *)

val echo : f:(float t -> float t) -> delay_s:float t -> float t -> float t
(** [echo ~f ~delay_s s] adds its own output to its input, modified by [f],
    after a delay of [delay_s] seconds. *)

val lazy_amplifier : float t -> volume:float t -> float t
val saturate : float t -> boost:float t -> threshold:float t -> float t
