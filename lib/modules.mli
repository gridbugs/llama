module Oscillator : sig
  type waveform = Sine
  type t = { waveform : waveform Signal.t; frequency_hz : float Signal.t }

  val signal : t -> float Signal.t
end
