open Llama
open Dsl

let () =
  let { Cli.sample_paths = _ } = Cli.parse () in
  Audio_io.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player
  := Presets.ambient () |> amplifier ~volume:(const 0.1);
  Lwt_main.run (Signal_player.run signal_player)
