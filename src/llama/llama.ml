include Llama_core

module Live = struct
  let go () =
    Llama_low_level.System.env_logger_init ();
    let signal_player = Signal_player.create () in
    let ref = Signal_player.signal_ref signal_player in
    let () = Lwt.async (fun () -> Signal_player.run signal_player) in
    ref

  include Dsl
  include Signal
end

let play_signal_lwt ?(downsample = 1) ?(scale_output_volume = 1.0) signal =
  Signal_player.play ~downsample (signal |> Dsl.scale scale_output_volume)

let play_signal ?(downsample = 1) ?(scale_output_volume = 1.0) signal =
  Lwt_main.run (play_signal_lwt ~downsample ~scale_output_volume signal)

module Signal_player = Signal_player
