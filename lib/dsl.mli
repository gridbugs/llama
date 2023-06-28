open! Modules
include Signal.Ops

module type S = sig
  val silence : float Signal.t

  type waveform = Oscillator.waveform = Sine | Saw

  val oscillator :
    waveform:waveform Signal.t -> frequency_hz:float Signal.t -> float Signal.t
end

include S
