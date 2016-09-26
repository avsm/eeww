#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "randomconv" @@ fun c ->
  Ok [ Pkg.mllib "src/randomconv.mllib" ]
