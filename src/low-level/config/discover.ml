module C = Configurator.V1

let macos_library_flags =
  let frameworks =
    [ "CoreServices"; "CoreAudio"; "CoreMidi"; "AudioUnit"; "AudioToolbox" ]
  in
  List.map (Printf.sprintf "-framework %s") frameworks

let () =
  C.main ~name:"llama_low_level" (fun c ->
      let linker_args =
        match C.ocaml_config_var_exn c "system" with
        | "macosx" -> macos_library_flags
        | "linux" -> (
            let default = [ "-lasound" ] in
            match C.Pkg_config.get c with
            | None -> default
            | Some pc -> (
                match C.Pkg_config.query pc ~package:"alsa" with
                | None -> default
                | Some conf -> conf.libs))
        | _ -> []
      in
      let cclib_arg = String.concat " " linker_args in
      C.Flags.write_sexp "library_flags.sexp" [ "-cclib"; cclib_arg ])
