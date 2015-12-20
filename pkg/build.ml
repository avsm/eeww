#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let alcotest = Env.bool "alcotest"
let () = Pkg.describe "hkdf" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "hkdf";
    Pkg.bin ~cond:alcotest ~auto:true "rfctests";
    Pkg.doc "README.md"; ]
