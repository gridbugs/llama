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
  ; the ocaml-boxroot-sys-0.2.0 crate doesn't build with ocaml 5
  (ocaml (< "5.0.0"))))
