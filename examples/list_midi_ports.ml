let () =
  let midi_input = Llama_low_level.Midi_input.create () in
  let midi_port_names =
    Llama_low_level.Midi_input.enumerate_midi_ports midi_input
  in
  List.iteri (fun i name -> Printf.printf "%d: %s\n" i name) midi_port_names
