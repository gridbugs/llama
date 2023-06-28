open! Modules
include Signal

let silence = const 0.0

type waveform = Oscillator.waveform = Sine | Saw

let oscillator ~waveform ~frequency_hz =
  Oscillator.signal { Oscillator.waveform; frequency_hz }

module type S = sig
  val silence : float Signal.t

  type waveform = Oscillator.waveform = Sine | Saw

  val oscillator :
    waveform:waveform Signal.t -> frequency_hz:float Signal.t -> float Signal.t
end
