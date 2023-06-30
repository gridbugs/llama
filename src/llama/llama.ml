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

let play_signal ?(downsample = 1) signal =
  Llama_low_level.System.env_logger_init ();
  let signal_player =
    Signal_player.create ~downsample ~initial_signal:signal ()
  in
  Lwt_main.run (Signal_player.run signal_player)

module Dsl = Dsl
module Signal_player = Signal_player
module Signal = Signal
module Float = Float
module Music = Music
module List = List
module Array = Array
