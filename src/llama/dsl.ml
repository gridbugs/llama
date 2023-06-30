open! Modules
include Signal

type waveform = Oscillator.waveform = Sine | Saw | Triangle | Square | Noise

let oscillator ?(square_wave_pulse_width_01 = const 0.5) waveform frequency_hz =
  Oscillator.(
    signal
      {
        waveform;
        frequency_hz;
        square_wave_pulse_width_01;
        reset_offset_01 = const 0.0;
        reset_trigger = const false;
      })

let noise_01 () = oscillator (const Noise) (const 0.0) |> to_01

let low_frequency_oscillator ?(square_wave_pulse_width_01 = const 0.5)
    ?(reset_offset_01 = const 0.0) waveform frequency_hz reset_trigger =
  Oscillator.(
    signal
      {
        waveform;
        frequency_hz;
        square_wave_pulse_width_01;
        reset_offset_01;
        reset_trigger;
      })

let low_frequency_oscillator_01 ?(square_wave_pulse_width_01 = const 0.5)
    ?(reset_offset_01 = const 0.0) waveform frequency_hz reset_trigger =
  low_frequency_oscillator ~square_wave_pulse_width_01 ~reset_offset_01 waveform
    frequency_hz reset_trigger
  |> to_01

let clock frequency_hz = Clock.(signal { frequency_hz })

let clock_divider denominator clock =
  Clock_divider.(signal { clock; denominator })

let asr_linear ~gate ~attack_s ~release_s =
  Asr_linear.(signal { gate; attack_s; release_s })

let adsr_linear ~gate ~attack_s ~decay_s ~sustain_01 ~release_s =
  Adsr_linear.(signal { gate; attack_s; decay_s; sustain_01; release_s })

type sequencer_output = Sequencer.output = {
  value : float Signal.t;
  gate : bool Signal.t;
}

type step_sequencer_step = Step_sequencer.step = {
  value : float Signal.t;
  period_s : float Signal.t;
}

let step_sequencer sequence clock = Step_sequencer.(signal { sequence; clock })

let random_sequencer values period clock =
  Random_sequencer.(signal { values; period; clock })

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

let chebyshev_low_pass_filter ?(filter_order_half = 1) signal_ ~cutoff_hz
    ~epsilon =
  Chebyshev_filter.(
    signal_low_pass { signal = signal_; cutoff_hz; epsilon } ~filter_order_half)

let chebyshev_high_pass_filter ?(filter_order_half = 1) signal_ ~cutoff_hz
    ~epsilon =
  Chebyshev_filter.(
    signal_high_pass { signal = signal_; cutoff_hz; epsilon } ~filter_order_half)

let sample_and_hold signal_ trigger =
  Sample_and_hold.(signal { signal = signal_; trigger })

let sample_player_mono data trigger =
  Sample_player_mono.(signal { data; trigger })

let bitwise_trigger_sequencer num_channels sequence clock =
  Bitwise_trigger_sequencer.(signals { num_channels; sequence; clock })

let delay signal_ ~time_s ~fill =
  Delay.(signal { signal = signal_; time_s; fill })

let clock_delay time_s clock = delay clock ~time_s:(const time_s) ~fill:false

let pulse ~frequency_hz ~duty_01 =
  oscillator ~square_wave_pulse_width_01:duty_01 (const Square) frequency_hz
  |> map ~f:(fun x -> x < 0.0)
