open! Modules
include Signal

type waveform = Oscillator.waveform = Sine

let oscillator ~waveform ~frequency_hz =
  Oscillator.signal { Oscillator.waveform; frequency_hz }
