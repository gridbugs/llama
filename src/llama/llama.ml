include Llama_core
module Player = Player

let play_signal ?buffer_size_in_samples ?viz signal =
  let player = Player.create () in
  let playing = Player.play_mono player ?buffer_size_in_samples ?viz signal in
  Player.Playing.wait playing

module Live = struct
  let go ?buffer_size_in_samples () =
    let signal = ref (Signal.const 0.0) in
    let player = Player.create () in
    let _playing =
      Player.play_mono player ?buffer_size_in_samples
        (Signal.of_signal_ref signal)
    in
    signal

  include Dsl
  include Signal
end

module Midi = struct
  include Midi

  let dummy_midi_messages = Signal.of_raw (fun _ -> [])
end
