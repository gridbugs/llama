open Llama
open Dsl

let random_pentatonic_sequencer octave period clock =
  let pentatonic o = [ (`C, o); (`D, o); (`F, o); (`G, o); (`A, o) ] in
  let notes = List.append (pentatonic (octave + 1)) (pentatonic octave) in
  let random_sequencer_values =
    List.map notes ~f:(fun note -> const (Music.Note.frequency_hz note))
  in
  random_sequencer random_sequencer_values (const period) clock

let pentatonic_strings ~sequencer_clock ~effect_clock:_ =
  Random.self_init ();
  let { value = sequencer_freq; gate } =
    random_pentatonic_sequencer 3 0.1 sequencer_clock
  in
  let osc_freq =
    sequencer_freq
    |> butterworth_low_pass_filter ~half_power_frequency_hz:(const 17.0)
  in
  let osc =
    mean
      [
        oscillator ~square_wave_pulse_width_01:(const 0.2) (const Saw) osc_freq;
      ]
  in
  let release_s = const 0.4 in
  let filter_env =
    adsr_linear ~gate ~attack_s:(const 0.01) ~decay_s:(const 0.2)
      ~sustain_01:(const 1.0) ~release_s
    |> exp01 4.0
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 1.0)
      ~cutoff_hz:(filter_env |> scale 500.0 |> offset 0.0)
    |> chebyshev_high_pass_filter ~epsilon:(const 1.0) ~cutoff_hz:(const 0.0)
  in
  filtered_osc
  *.. (asr_linear ~gate ~attack_s:(const 0.01) ~release_s |> exp01 1.0)
  |> map ~f:(fun x -> Float.clamp_1 (x *. 1.0))

let pentatonic_overdrive ~sequencer_clock ~effect_clock =
  Random.self_init ();
  let _sequencer_clock = effect_clock |> clock_divider 4 in
  let noise = noise_01 () in
  let sah_noise = sample_and_hold noise effect_clock in
  let { value = sequencer_freq; gate } =
    random_pentatonic_sequencer 2 0.1 sequencer_clock
  in
  let lfo =
    low_frequency_oscillator_01 (const Sine) (const (1.0 /. 13.0)) never
  in
  let osc_freq = sequencer_freq in
  let osc =
    mean
      [
        oscillator ~square_wave_pulse_width_01:(const 0.2) (const Square)
          osc_freq;
        oscillator ~square_wave_pulse_width_01:(const 0.2) (const Square)
          (osc_freq |> scale 1.5);
      ]
  in
  let release_s = const 1.0 in
  let filter_env =
    adsr_linear ~gate ~attack_s:(const 0.1) ~decay_s:(const 0.1)
      ~sustain_01:(const 1.0) ~release_s
    |> exp01 1.0
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 10.0)
      ~cutoff_hz:
        ((filter_env |> scale 500.0 |> offset 200.0)
        +.. (lfo |> scale 2000.0)
        +.. (sah_noise |> scale 1000.0))
    |> chebyshev_high_pass_filter ~epsilon:(const 4.0) ~cutoff_hz:(const 100.0)
  in
  filtered_osc *.. asr_linear ~gate ~attack_s:(const 0.01) ~release_s
  |> map ~f:(fun x -> Float.clamp_1 (x *. 3.0))
