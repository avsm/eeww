(lang dune 2.0)

(name eio-ssl)

(generate_opam_files true)

(package
 (name eio-ssl)
 (maintainers "Antonio Nuno Monteiro <anmonteiro@gmail.com>")
 (authors "Antonio Nuno Monteiro <anmonteiro@gmail.com>")
 (homepage "https://github.com/anmonteiro/eio-ssl")
 (bug_reports "https://github.com/anmonteiro/eio-ssl/issues")
 (license "LGPL with OpenSSL linking exception")
 (source
  (github melange-re/melange-compiler-libs))
 (synopsis "OpenSSL binding to EIO")
 (depends
  (ocaml
   (>= "5.0"))
  (ssl
   (>= "0.5.13"))
  eio
  eio_main))
