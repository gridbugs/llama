open! Modules

include module type of struct
  include Signal
end

type waveform = Sine | Saw | Triangle | Square | Noise

val oscillator :
  ?square_wave_pulse_width_01:float t -> waveform t -> float t -> float t

val noise_01 : unit -> float t
val noise : min:float t -> max:float t -> float t

val low_frequency_oscillator :
  ?square_wave_pulse_width_01:float t ->
  ?reset_offset_01:float t ->
  waveform t ->
  float t ->
  bool t ->
  float t

val low_frequency_oscillator_01 :
  ?square_wave_pulse_width_01:float t ->
  ?reset_offset_01:float t ->
  waveform t ->
  float t ->
  bool t ->
  float t

val clock : float t -> bool t
val clock_of_period_s : float t -> bool t
val clock_divide : int -> bool t -> bool t
val asr_linear : gate:bool t -> attack_s:float t -> release_s:float t -> float t

val adsr_linear :
  gate:bool t ->
  attack_s:float t ->
  decay_s:float t ->
  sustain_01:float t ->
  release_s:float t ->
  float t

type sequencer_output = { value : float Signal.t; gate : bool Signal.t }
type step_sequencer_step = { value : float Signal.t; period_s : float Signal.t }

val step_sequencer :
  step_sequencer_step option list -> bool t -> sequencer_output

val random_sequencer : float t list -> float t -> bool t -> sequencer_output

val butterworth_low_pass_filter :
  ?filter_order_half:int ->
  float t ->
  half_power_frequency_hz:float t ->
  float t

val butterworth_high_pass_filter :
  ?filter_order_half:int ->
  float t ->
  half_power_frequency_hz:float t ->
  float t

val chebyshev_low_pass_filter :
  ?filter_order_half:int ->
  float t ->
  cutoff_hz:float t ->
  epsilon:float t ->
  float t

val chebyshev_high_pass_filter :
  ?filter_order_half:int ->
  float t ->
  cutoff_hz:float t ->
  epsilon:float t ->
  float t

val sample_and_hold : float t -> bool t -> float t
val sample_player_mono : float array -> bool t -> float t
val bitwise_trigger_sequencer : int -> int t list -> bool t -> bool t list
val delay : 'a t -> time_s:float t -> fill:'a -> 'a t
val clock_delay : float -> bool t -> bool t
val pulse : frequency_hz:float t -> duty_01:float t -> bool t

val feedback : f:(float t -> float t) -> float t -> float t
(** [feedback ~f s] adds its previous output modifier by [f] to its input. *)

val echo : f:(float t -> float t) -> delay_s:float t -> float t -> float t
(** [echo ~f ~delay_s s] adds its own output to its input, modified by [f],
    after a delay of [delay_s] seconds. *)
