open StdLabels
open Llama

module Args = struct
  type t = { list_midi_ports : bool; midi_port : int }

  let parse () =
    let list_midi_ports = ref false in
    let midi_port = ref 0 in
    Arg.parse
      [
        ( "--list-midi-ports",
          Arg.Set list_midi_ports,
          "List midi ports by index and exit" );
        ( "--midi-port",
          Arg.Set_int midi_port,
          "Use the midi port with this index" );
      ]
      (fun anon_arg ->
        failwith (Printf.sprintf "Unexpected position argument: %s" anon_arg))
      "Play music with a midi keyboard";
    { list_midi_ports = !list_midi_ports; midi_port = !midi_port }
end

let () =
  let { Args.list_midi_ports; midi_port } = Args.parse () in
  let midi_input = Llama.Midi.Midi_input.create () in
  if list_midi_ports then
    let midi_port_names = Llama.Midi.Midi_input.port_names midi_input in
    List.iteri
      ~f:(fun i name -> Printf.printf "%d: %s\n" i name)
      midi_port_names
  else
    let midi_events =
      Midi.live_midi_signal midi_input midi_port |> Result.get_ok
    in
    Signal.debug midi_events
      ~f:
        (List.iter ~f:(fun event -> print_endline (Midi.Event.to_string event)))
    |> Signal.map ~f:(Fun.const 0.0)
    |> play_signal
