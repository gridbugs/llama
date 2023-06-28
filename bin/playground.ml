open Llama

let make_signal () =
  let open Dsl in
  oscillator ~waveform:(const Sine) ~frequency_hz:(const 440.0)

let () =
  Audio_io.System.env_logger_init ();
  let signal_player = Signal_player.create ~downsample:1 () in
  Signal_player.signal_ref signal_player := make_signal ();
  Lwt_main.run (Signal_player.run signal_player)
