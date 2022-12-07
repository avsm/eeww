(lang dune 3.4)
(name eeww)

(generate_opam_files true)

(source (github avsm/eeww))
(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "anil@recoil.org")

(package
 (name eeww)
 (synopsis "")
 (description "")
 (depends cohttp-eio eio_luv eio_main eio uri fpath magic-mime))
