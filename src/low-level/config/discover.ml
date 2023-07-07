module C = Configurator.V1

let macos_library_flags =
  let frameworks =
    [ "CoreServices"; "CoreAudio"; "AudioUnit"; "AudioToolbox" ]
  in
  let cclib_arg =
    List.map (Printf.sprintf "-framework %s") frameworks |> String.concat " "
  in
  [ "-cclib"; Printf.sprintf "\"%s\"" cclib_arg ]

let () =
  C.main ~name:"llama_low_level" (fun _c ->
      C.Flags.write_sexp "library_flags.sexp" macos_library_flags)
