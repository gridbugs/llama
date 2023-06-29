open! Modules

include module type of struct
  include Signal
end

val silence : float t

type waveform = Sine | Saw

val oscillator : waveform t -> float t -> float t
val clock : float t -> bool t
val amplifier : float t -> volume:float t -> float t
val asr_linear : gate:bool t -> attack_s:float t -> release_s:float t -> float t

val adsr_linear :
  gate:bool t ->
  attack_s:float t ->
  decay_s:float t ->
  sustain_01:float t ->
  release_s:float t ->
  float t

type step_sequencer_step = { value : float Signal.t; period_s : float Signal.t }
type step_sequencer_output = { value : float Signal.t; gate : bool Signal.t }

val step_sequencer :
  step_sequencer_step option list -> bool t -> step_sequencer_output

val butterworth_low_pass_filter :
  ?filter_order_half:int ->
  float t ->
  half_power_frequency_hz:float t ->
  float t
