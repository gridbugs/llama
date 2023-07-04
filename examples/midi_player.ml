module Args = struct
  type t = { midi_file_path : string }

  let usage () =
    let arg0 =
      if Array.length Sys.argv < 1 then "midi.exe" else Array.get Sys.argv 0
    in
    Printf.sprintf "USAGE:\n%s FILE\n\nPlay a midi file.\n" arg0

  let parse () =
    let anon_args = ref [] in
    let spec = [] in
    Arg.parse spec
      (fun anon_arg -> anon_args := anon_arg :: !anon_args)
      (usage ());
    match !anon_args with
    | [ midi_file_path ] -> { midi_file_path }
    | [] ->
        Printf.eprintf "Missing midi file path!\n\n%s"
          (Arg.usage_string spec (usage ()));
        exit 1
    | _ ->
        Printf.eprintf "Too many anonymous arguments!\n\n%s"
          (Arg.usage_string spec (usage ()));
        exit 1
end

let () =
  let { Args.midi_file_path } = Args.parse () in
  let reader = Llama_midi.File_reader.of_path midi_file_path in
  let data = Llama_midi.File_reader.read reader in
  print_endline (Llama_midi.Data.to_string data);
  print_endline "x"
