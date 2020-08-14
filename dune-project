(lang dune 2.6)

(name kqueue)

(generate_opam_files true)

(source
 (github anuragsoni/kqueue))

(authors "Anurag Soni")

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(license BSD-3-clause)

(package
 (name kqueue)
 (depends
  (ocaml
   (>= 4.08))
  ctypes
  sexplib0
  ppx_sexp_conv))
