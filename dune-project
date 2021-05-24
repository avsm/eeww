(lang dune 2.8)

(name network)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-network))

(license ISC)

(authors "Patrick Ferris")

(maintainers "pf341@patricoferris.com")

(package
 (name network)
 (synopsis "OCaml bindings for Apple's Network Framework")
 (description "Bindings for Apple's Network Framework.")
 (depends
  dispatch
  (logs :with-test)
  (fmt :with-test)
  (cmdliner :with-test)))
