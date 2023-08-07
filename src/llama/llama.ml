open StdLabels
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

module Midi = struct
  include Midi

  module Midi_input = struct
    type t = Llama_low_level.Midi_input.t

    let create = Llama_low_level.Midi_input.create
    let port_names = Llama_low_level.Midi_input.port_names
  end

  let live_midi_signal input port =
    if port < Llama_low_level.Midi_input.get_num_ports input then (
      Llama_low_level.Midi_input.port_connect input port;
      Ok
        (Signal.of_raw (fun _ ->
             let raw_data =
               Llama_low_level.Midi_input.port_drain_messages_to_char_array
                 input
             in
             Midi.Event.parse_multi_from_char_array raw_data)))
    else Error `No_such_port

  let live_midi_signal_messages input port =
    live_midi_signal input port
    |> Result.map (fun event_signal ->
           Signal.map event_signal
             ~f:(List.map ~f:(fun (event : Event.t) -> event.message)))

  let live_midi_sequencer input ~port ~channel ~polyphony =
    live_midi_signal_messages input port
    |> Result.map (Midi_sequencer.signal ~channel ~polyphony)

  let live_midi_messages_serial ~port ~baud =
    let midi_serial = Midi_serial.create ~port ~baud in
    Signal.of_raw (fun _ ->
        Midi_serial.consume_all_available_bytes midi_serial;
        Midi_serial.drain_messages midi_serial)
end
