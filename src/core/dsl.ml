open StdLabels
open Modules
include Signal

type waveform = Oscillator.waveform = Sine | Saw | Triangle | Pulse | Noise

let waveform_to_string = Oscillator.waveform_to_string

let oscillator ?(pulse_width_01 = const 0.5)
    ?(reset_trigger = Signal.Trigger.never) waveform frequency_hz =
  Oscillator.(
    signal
      {
        waveform;
        frequency_hz;
        pulse_width_01;
        reset_offset_01 = const 0.0;
        reset_trigger;
      })

let noise_01 () = oscillator (const Noise) (const 0.0) |> to_01

let noise ~min ~max =
  both (noise_01 ()) (both max min)
  |> map ~f:(fun (noise, (max, min)) -> min +. (noise *. (max -. min)))

let noise_1 () = noise ~min:(const (-1.0)) ~max:(const 1.0)

let low_frequency_oscillator ?(pulse_width_01 = const 0.5)
    ?(reset_offset_01 = const 0.0) waveform frequency_hz reset_trigger =
  Oscillator.(
    signal
      { waveform; frequency_hz; pulse_width_01; reset_offset_01; reset_trigger })

let low_frequency_oscillator_01 ?(pulse_width_01 = const 0.5)
    ?(reset_offset_01 = const 0.0) waveform frequency_hz reset_trigger =
  low_frequency_oscillator ~pulse_width_01 ~reset_offset_01 waveform
    frequency_hz reset_trigger
  |> to_01

let clock_of_frequency_hz frequency_hz = Clock.(trigger { frequency_hz })
let clock_of_period_s period_s = clock_of_frequency_hz (recip period_s)

let clock_divide denominator clock =
  Clock_divider.(trigger { clock; denominator })

let ar_linear ~gate ~attack_s ~release_s =
  Ar_linear.(signal { gate; attack_s; release_s })

let adsr_linear ~gate ~attack_s ~decay_s ~sustain_01 ~release_s =
  Adsr_linear.(signal { gate; attack_s; decay_s; sustain_01; release_s })

type 'a sequencer_output = 'a Sequencer.output = {
  value : 'a Signal.t;
  gate : Signal.Gate.t;
}

type 'a sequencer_step = 'a Sequencer.step = {
  value : 'a Signal.t;
  period_s : float Signal.t;
}

let sustained_step_sequencer sequence clock =
  Sustained_step_sequencer.(signal { sequence; clock })

let generic_step_sequencer sequence clock =
  Generic_step_sequencer.(signal { sequence; clock })

let random_sequencer values period clock =
  Random_sequencer.(signal { values; period; clock })

let value_sequencer values clock =
  let { value; gate = _ } =
    generic_step_sequencer
      (List.map values ~f:(fun value -> { value; period_s = const 0.0 }))
      clock
  in
  value

let butterworth_low_pass_filter ?(filter_order_half = 1) signal_ ~cutoff_hz =
  Butterworth_filter.(
    signal_low_pass { signal = signal_; cutoff_hz } ~filter_order_half)

let butterworth_high_pass_filter ?(filter_order_half = 1) signal_ ~cutoff_hz =
  Butterworth_filter.(
    signal_high_pass { signal = signal_; cutoff_hz } ~filter_order_half)

let chebyshev_low_pass_filter ?(filter_order_half = 1) signal_ ~cutoff_hz
    ~resonance =
  Chebyshev_filter.(
    signal_low_pass
      { signal = signal_; cutoff_hz; resonance }
      ~filter_order_half)

let chebyshev_high_pass_filter ?(filter_order_half = 1) signal_ ~cutoff_hz
    ~resonance =
  Chebyshev_filter.(
    signal_high_pass
      { signal = signal_; cutoff_hz; resonance }
      ~filter_order_half)

let sample_and_hold signal_ trigger =
  Sample_and_hold.(signal { signal = signal_; trigger })

let sample_player_mono data trigger =
  Sample_player_mono.(signal { data; trigger })

let bitwise_trigger_sequencer num_channels sequence clock =
  Bitwise_trigger_sequencer.(triggers { num_channels; sequence; clock })

let delay signal_ ~time_s ~fill =
  Delay.(signal { signal = signal_; time_s; fill })

let clock_delay time_s clock =
  delay (Signal.Trigger.to_signal clock) ~time_s:(const time_s) ~fill:false
  |> Signal.Trigger.of_signal_unsafe

let periodic_gate ~frequency_hz ~duty_01 =
  oscillator ~pulse_width_01:duty_01 (const Pulse) frequency_hz
  |> map ~f:(fun x -> x < 0.0)
  |> Signal.Gate.of_signal

let feedback ~f =
  let previous_output_signal, previous_output_ref = var 0.0 in
  fun input_signal ->
    map
      (f previous_output_signal +.. input_signal)
      ~f:(fun x ->
        previous_output_ref := x;
        x)

let echo ~f ~delay_s =
  feedback ~f:(fun out -> delay (f out) ~time_s:delay_s ~fill:0.0)

let lazy_amplifier signal_ ~volume =
  Lazy_amplifier.(signal { signal = signal_; volume })

let saturate signal_ ~boost ~threshold =
  map3 signal_ boost threshold ~f:(fun x boost threshold ->
      x *. boost |> Float.clamp_sym ~mag:threshold)
