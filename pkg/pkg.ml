#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let coverage = Conf.with_pkg ~default:false "coverage"

let cmd c os files =
  let build =
    if Conf.value c coverage then
      let coverage_arg = Cmd.(v "-pkg" % "bisect_ppx") in
      let coverage_cmd c os = Cmd.(Pkg.build_cmd c os %% coverage_arg) in
      coverage_cmd
    else
      Pkg.build_cmd
  in
  OS.Cmd.run @@ Cmd.(build c os %% of_list files)

let () =
  Pkg.describe ~build:(Pkg.build ~cmd ()) "domain-name" @@ fun _ ->
  Ok [
    Pkg.mllib "src/domain-name.mllib" ;
    Pkg.test "test/tests" ;
  ]
