(lang dune 3.0)

(generate_opam_files true)
(name llama)

(source (github gridbugs/llama))
(license MIT)
(authors "Stephen Sherratt")
(maintainers "https://github.com/gridbugs/llama/issues")

(package
 (name llama_midi)
 (synopsis "Minimal library for parsing and representing midi data")
 (depends
  (ocaml (>= "4.13"))))

(package
 (name llama_core)
 (synopsis "Core types and operations for the Llama synthesizer library")
 (description "Core types and operations for Llama. This is separate from the llama package so that other packages can be written which define new synthesizer modules based on llama without needing to depend on additional packages required by llama to interact with the sound card.")
 (depends
  (ocaml (>= "4.13"))
  (llama_midi (= :version)))
 (conflicts
   (result (< "1.5"))))

(package
 (name llama)
 (synopsis "Language for Live Audio Module Arrangement")
 (description "Libraries for declaratively building software-defined modular synthesizers. Contains definitions of common modules for generating and processing audio waveforms such as oscillators, filters, envelope generators, sequencers and clocks. This package also includes a mechanism for playing sounds generated by synthesizer components through your speakers.")
 (depends

  (llama_core (= :version))

  ; The ocaml-boxroot-sys-0.2.0 crate doesn't build with ocaml 5.
  ; Some numerical operations only became available in 4.13.
  (ocaml (and (>= "4.13.0") (< "5.0.0")))

  (lwt (>= 5.4))

  (conf-rust-2021 :build)

  (conf-pkg-config (and :build (= :os linux)))
  (dune-configurator :build)

  ; On linux, "alsa" is a dependency of the rust package "cpal" and we can use
  ; opam to install it for us.
  (conf-alsa (= :os linux))))

(package
 (name llama_interactive)
 (synopsis "Visualization and live interaction for Llama synthesizer library")
 (depends
  (ocaml (>= "4.13"))
  (llama (= :version))
  (tsdl (>= 1))
  (conf-pkg-config :build)))

(package
 (name llama_tests)
 (synopsis "Tests for llama packages")
 (depends
  (ocaml (>= "4.13"))
  (llama_midi (= :version))
  ppx_inline_test))
