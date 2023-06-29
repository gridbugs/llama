open Llama
open Dsl

let c = 130.81
let e = 164.81

let hsc_sequence =
  [
    Some (c, 1.5);
    None;
    Some (c, 3.0);
    None;
    None;
    Some (c, 0.5);
    Some (e, 0.5);
    Some (c, 0.5);
    Some (e, 0.5);
    Some (c, 1.5);
    None;
    Some (e, 0.5);
    Some (c, 1.5);
    None;
    None;
    None;
  ]

let short_sequence = [ Some (c /. 1.0, 0.1); None ]

let make_signal () =
  let sequencer_clock_freq = 0.1 in
  let effect_clock_freq = sequencer_clock_freq *. 64.0 in
  let sequencer_clock = clock (const sequencer_clock_freq) in
  let effect_clock = clock (const effect_clock_freq) in
  let noise = noise_01 () in
  let sah_noise = sample_and_hold noise effect_clock in
  let { value = sequencer_freq; gate } =
    step_sequencer
      (List.map short_sequence
         ~f:
           (Option.map (fun (freq, period) ->
                {
                  value = const freq;
                  period_s = const (period /. sequencer_clock_freq);
                })))
      sequencer_clock
  in
  let lfo =
    low_frequency_oscillator_01 (const Sine)
      (const (sequencer_clock_freq /. 17.0))
      never
  in
  let lfo2 =
    low_frequency_oscillator_01 (const Sine)
      (const (sequencer_clock_freq /. 9.0))
      never
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
  let release_s = const 20.0 in
  let filter_env =
    adsr_linear ~gate ~attack_s:(const 10.0) ~decay_s:(const 0.1)
      ~sustain_01:(const 1.0) ~release_s
    |> exp01 1.0
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 10.0)
      ~cutoff_hz:
        ((filter_env |> scale 1000.0 |> offset 0.0)
        +. (lfo |> scale 2000.0)
        +. (sah_noise |> scale 1000.0))
    |> chebyshev_high_pass_filter ~epsilon:(const 1.0)
         ~cutoff_hz:(lfo2 |> scale 400.0)
  in
  amplifier filtered_osc
    ~volume:(asr_linear ~gate ~attack_s:(const 0.01) ~release_s)
  |> map ~f:(fun x -> Float.clamp_1 (x *. 2.0))

let () =
  Audio_io.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player
  := make_signal () |> amplifier ~volume:(const 0.1);
  Lwt_main.run (Signal_player.run signal_player)
