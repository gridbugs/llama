(* Example that plays random notes from the pentatonic scale with several
   filters and effects applied. If you pass the paths of 3 wav files they will
   be interpreted as samples of a cymbal, snare, and bass respectively, and an
   additional drum track will be played.

   E.g.:  dune exec ./examples/random_pentatonic.exe -- cymbal.wav snare.wav bass.wav
*)

open Llama
open Dsl

let random_pentatonic_sequencer octave period clock =
  let pentatonic o = [ (`C, o); (`D, o); (`F, o); (`G, o); (`A, o) ] in
  let notes = List.append (pentatonic (octave + 1)) (pentatonic octave) in
  let random_sequencer_values =
    List.map notes ~f:(fun note -> const (Music.Note.frequency_hz note))
  in
  random_sequencer random_sequencer_values (const period) clock

let pentatonic_overdrive ~sequencer_clock ~effect_clock =
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
    |> exp_01 1.0
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

let drum_machine ~cymbal:cymbal_p ~snare:snare_p ~bass:bass_p clock =
  let cymbal = 1 lsl 0 in
  let snare = 1 lsl 1 in
  let bass = 1 lsl 2 in
  let sequence =
    [
      snare lor bass;
      0;
      snare lor cymbal;
      0;
      snare;
      snare lor bass;
      snare lor cymbal;
      0;
    ]
    |> List.map ~f:const
  in
  let cymbal_c, snare_c, bass_c =
    match bitwise_trigger_sequencer 3 sequence clock with
    | [ a; b; c ] -> (a, b, c)
    | _ -> failwith "unreachable"
  in
  sum [ cymbal_p cymbal_c; snare_p snare_c; bass_p bass_c ]

let pentatonic_strings ~sequencer_clock =
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
      ~sustain_01:(const 0.0) ~release_s
    |> exp_01 4.0
  in
  let filtered_osc =
    chebyshev_low_pass_filter osc ~epsilon:(const 1.0)
      ~cutoff_hz:(filter_env |> scale 4000.0 |> offset 0.0)
  in
  filtered_osc
  *.. (asr_linear ~gate ~attack_s:(const 0.01) ~release_s |> exp_01 1.0)
  |> map ~f:(fun x -> Float.clamp_1 (x *. 1.0))

let play_possibly_with_drums wav_players which =
  let drum_machine clock =
    match wav_players with
    | [ cymbal; snare; bass ] -> drum_machine ~cymbal ~snare ~bass clock
    | [ cymbal; snare ] ->
        drum_machine ~cymbal ~snare ~bass:(Fun.const silence) clock
    | [ cymbal ] ->
        drum_machine ~cymbal ~snare:(Fun.const silence)
          ~bass:(Fun.const silence) clock
    | _ -> silence
  in
  let clock = clock (const 16.0) in
  match which with
  | `Overdrive ->
      drum_machine (clock |> clock_divide 4)
      +.. (pentatonic_overdrive
             ~sequencer_clock:(clock |> clock_divide 8)
             ~effect_clock:(clock |> clock_divide 2)
          |> scale 0.4)
  | `Strings ->
      drum_machine (clock |> clock_divide 4)
      +.. pentatonic_strings ~sequencer_clock:(clock |> clock_divide 2)

module Args = struct
  type t = { sample_paths : string list; strings : bool }

  let parse () =
    let sample_paths = ref [] in
    let strings = ref false in
    Arg.parse
      [
        ( "--strings",
          Arg.Set strings,
          "Play the \"strings\" demo rather than the default" );
      ]
      (fun anon_arg -> sample_paths := anon_arg :: !sample_paths)
      "Play random pentatonic notes with filters and effects, and optionally \
       drums if wav files are provided.";
    { sample_paths = List.rev !sample_paths; strings = !strings }
end

let () =
  let { Args.sample_paths; strings } = Args.parse () in
  let wav_players =
    List.map sample_paths ~f:(fun sample_path ->
        Llama_io.Wav.(of_file_at_path sample_path |> sample_player_mono))
  in
  let which = if strings then `Strings else `Overdrive in
  Random.self_init ();
  play_signal (play_possibly_with_drums wav_players which |> scale 0.1)
