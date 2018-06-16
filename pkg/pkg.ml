#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "domain-name" @@ fun _ ->
  Ok [
    Pkg.mllib "src/domain-name.mllib" ;
    Pkg.test "test/tests" ;
  ]
