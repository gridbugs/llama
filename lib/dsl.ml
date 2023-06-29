open! Modules
include Signal

let silence = const 0.0

type waveform = Oscillator.waveform = Sine | Saw

let oscillator waveform frequency_hz =
  Oscillator.(signal { waveform; frequency_hz })

let clock frequency_hz = Clock.(signal { frequency_hz })
let amplifier signal_ ~volume = Amplifier.(signal { signal = signal_; volume })

let asr_linear ~gate ~attack_s ~release_s =
  Asr_linear.(signal { gate; attack_s; release_s })

let adsr_linear ~gate ~attack_s ~decay_s ~sustain_01 ~release_s =
  Adsr_linear.(signal { gate; attack_s; decay_s; sustain_01; release_s })

type step_sequencer_step = Step_sequencer.step = {
  value : float Signal.t;
  period_s : float Signal.t;
}

type step_sequencer_output = Step_sequencer.output = {
  value : float Signal.t;
  gate : bool Signal.t;
}

let step_sequencer sequence clock = Step_sequencer.(signal { sequence; clock })

let butterworth_low_pass_filter ?(filter_order_half = 1) signal_
    ~half_power_frequency_hz =
  Butterworth_filter.(
    signal_low_pass
      { signal = signal_; half_power_frequency_hz }
      ~filter_order_half)

let butterworth_high_pass_filter ?(filter_order_half = 1) signal_
    ~half_power_frequency_hz =
  Butterworth_filter.(
    signal_high_pass
      { signal = signal_; half_power_frequency_hz }
      ~filter_order_half)
