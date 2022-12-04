(lang dune 3.6)

(name realget)

(generate_opam_files true)

(source
 (github realget-lang/realget-llvm))

(authors "Chayut Saengjan" "Arituch Chinnapan")

(maintainers "gunnchayut@gmail.com" "icarituch@gmail.com")

(license Apache-2.0)

(package
 (name realget)
 (synopsis "The realget programming language")
 (depends
  ocaml
  dune
  qcheck
  qcheck-alcotest
  alcotest
  core
  ocamlformat
  bisect-ppx
  ppx-tools-versioned
  ppx-jane
  fmt
  menhir
  )
)

(using menhir 2.0)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
