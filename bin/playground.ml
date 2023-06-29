open Llama
open Dsl

let make_sequencer clock =
  let pentatonic o = [ (`C, o); (`D, o); (`F, o); (`G, o); (`A, o) ] in
  let notes = List.append (pentatonic 3) (pentatonic 2) in
  let random_sequencer_values =
    List.map notes ~f:(fun note -> const (Music.Note.frequency_hz note))
  in
  random_sequencer random_sequencer_values (const 0.01) clock

let _make_sequencer clock =
  let open Sequence in
  let sequence = to_steps middle_c_loop ~time_scale:1.0 in
  step_sequencer sequence clock

let make_signal () =
  Random.self_init ();
  let sequencer_clock_freq = 4.0 in
  let effect_clock_freq = 4.0 in
  let sequencer_clock = clock (const sequencer_clock_freq) in
  let effect_clock = clock (const effect_clock_freq) in
  let noise = noise_01 () in
  let sah_noise = sample_and_hold noise effect_clock in
  let { value = sequencer_freq; gate } = make_sequencer sequencer_clock in
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
        oscillator ~square_wave_pulse_width_01:(const 0.2) (const Saw) osc_freq;
        (* oscillator ~square_wave_pulse_width_01:(const 0.2) (const Saw)
           (osc_freq |> scale 0.5); *)
      ]
  in
  let release_s = const 0.4 in
  let filter_env =
    adsr_linear ~gate ~attack_s:(const 0.01) ~decay_s:(const 0.2)
      ~sustain_01:(const 1.0) ~release_s
    |> exp01 4.0
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 0.01)
      ~cutoff_hz:(filter_env |> scale 500.0 |> offset 0.0)
    |> chebyshev_high_pass_filter ~epsilon:(const 1.0) ~cutoff_hz:(const 0.0)
  in
  amplifier filtered_osc
    ~volume:(asr_linear ~gate ~attack_s:(const 0.01) ~release_s |> exp01 1.0)
  |> map ~f:(fun x -> Float.clamp_1 (x *. 1.0))

let () =
  Audio_io.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player
  := make_signal () |> amplifier ~volume:(const 0.1);
  Lwt_main.run (Signal_player.run signal_player)
