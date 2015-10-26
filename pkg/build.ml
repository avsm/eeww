#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "uucp" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/uucp";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "DEVEL.md";
    Pkg.doc "test/examples.ml";
  ]
