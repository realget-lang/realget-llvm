(lang dune 3.6)

(name realget)

(generate_opam_files false)

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
  fmt
  menhir
  )
)

(using menhir 2.0)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
