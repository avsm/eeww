#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "asetmap" @@ fun c ->
  Ok [ Pkg.mllib "src/asetmap.mllib";
       Pkg.test "test/test"; ]
