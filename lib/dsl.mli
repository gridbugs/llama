open! Modules
include Signal.Ops

type waveform = Oscillator.waveform = Sine

val oscillator :
  waveform:waveform Signal.t -> frequency_hz:float Signal.t -> float Signal.t
