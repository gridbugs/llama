type t = { sample_paths : string list }

let parse () =
  let sample_paths = ref [] in
  Arg.parse [] (fun anon_arg -> sample_paths := anon_arg :: !sample_paths) "foo";
  { sample_paths = List.rev !sample_paths }
