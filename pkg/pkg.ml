#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "hkdf" @@ fun _c ->
  Ok [
    Pkg.mllib "hkdf.mllib";
    Pkg.test "rfctests"
  ]
