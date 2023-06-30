open Llama
open Dsl

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
      0;
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

let wav_test wav_players =
  let drum_machine clock =
    match wav_players with
    | [ cymbal; snare; bass ] -> drum_machine ~cymbal ~snare ~bass clock
    | _ -> silence
  in
  let clock = clock (const 16.0) in
  drum_machine (clock |> clock_divider 4)
  +.. (Presets.pentatonic_overdrive
         ~sequencer_clock:(clock |> clock_divider 4)
         ~effect_clock:(clock |> clock_divider 2)
      |> scale 0.6)

let () =
  let { Cli.sample_paths } = Cli.parse () in
  let wav_players =
    List.map sample_paths ~f:(fun sample_path ->
        Llama_io.Wav.(of_file_at_path sample_path |> sample_player_mono))
  in
  Llama_low_level.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player := wav_test wav_players;
  Lwt_main.run (Signal_player.run signal_player)
