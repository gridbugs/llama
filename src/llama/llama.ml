module Live = struct
  let go () =
    Audio_io.System.env_logger_init ();
    let signal_player = Signal_player.create () in
    let ref = Signal_player.signal_ref signal_player in
    let () = Lwt.async (fun () -> Signal_player.run signal_player) in
    ref

  include Dsl
  include Signal
end

module Dsl = Dsl
module Signal_player = Signal_player
module Signal = Signal
module Float = Float
module Music = Music
module List = List
module Array = Array
