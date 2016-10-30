#!/usr/bin/env ocaml
#directory "pkg"
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "pbkdf" @@ fun _c ->
  Ok [
    Pkg.mllib "pbkdf.mllib";
    Pkg.test "pbkdf_tests"
  ]
