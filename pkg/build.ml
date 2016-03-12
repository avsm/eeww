#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let alcotest = Env.bool "alcotest"
let () = Pkg.describe "pbkdf" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "pbkdf";
    Pkg.bin ~cond:alcotest ~auto:true "pbkdf_tests";
    Pkg.doc "README.md"; ]
