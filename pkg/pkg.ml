#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "durations" @@ fun c ->
  Ok [
    Pkg.mllib "src/durations.mllib";
  ]
