(lang dune 3.0)

(generate_opam_files true)
(name llama)

(source (github gridbugs/llama))
(license MIT)
(authors "Stephen Sherratt")
(maintainers "https://github.com/gridbugs/llama/issues")

(package
 (name llama)
 (synopsis "Language for Live Audio Module Arrangement")
 (depends

  ; The ocaml-boxroot-sys-0.2.0 crate doesn't build with ocaml 5.
  (ocaml (< "5.0.0"))

  (lwt (>= 5.4))

  ; Many rust users never install rust with their system package manager and
  ; instead use the rustup tool to manage rust installations. Thus we want to
  ; cater for the case where a user doesn't want to install rust globally but
  ; still wants to use this tool, but it would also be useful to support the
  ; case where a user wants 'opam install llama' to just work without having to
  ; think about the fact that it depends on rust. To cater for both cases we'll
  ; release a version of this package which explicitly depends on
  ; "conf-rust-2021" and a second version which doesn't. By default in the opam
  ; file checked into git we won't depend on rust so that users who clone this
  ; repo can manage their own rust installation.
  ; conf-rust-2021

  ; On linux, "alsa" is a dependency of the rust package "cpal" and we can use
  ; opam to install it for us.
  (conf-alsa (= :os linux))))
