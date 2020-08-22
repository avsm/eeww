(lang dune 2.6)

(name kqueue)

(generate_opam_files true)

(source
 (github anuragsoni/kqueue-ml))

(authors "Anurag Soni")

(maintainers "Anurag Soni <anurag@sonianurag.com>")

(documentation https://anuragsoni.github.io/kqueue-ml)

(license BSD-3-clause)

(package
 (name kqueue)
 (synopsis "OCaml bindings for kqueue event notification interface")
 (tags (kqueue))
 (depends
  (ocaml
   (>= 4.08))
  ctypes))
