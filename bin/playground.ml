open Llama
open Dsl

let wav_test wav_players =
  let trigger = clock (const 1.0) in
  (List.hd wav_players) trigger

let () =
  let { Cli.sample_paths } = Cli.parse () in
  let wav_players =
    List.map sample_paths ~f:(fun sample_path ->
        Llama_io.Wav.(of_file_at_path sample_path |> sample_player_mono))
  in
  Llama_low_level.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player := wav_test wav_players;
  (* Presets.ambient () |> amplifier ~volume:(const 0.1);*)
  Lwt_main.run (Signal_player.run signal_player)
