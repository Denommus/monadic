(lang dune 3.16)

(name monadic)

(using melange 0.1)

(license "MIT")

(maintainers "Denommus")

(generate_opam_files)

(package
 (name monadic)
 (synopsis "A monad transformers library")
 (description "A monad transformers library")
 (depends
  (ocaml (>= 5.0))
  melange
  ounit))
