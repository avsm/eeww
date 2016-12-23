#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
  Pkg.meta_file ~install:false "pkg/META.lwt";
  Pkg.meta_file ~install:false "pkg/META.unix";
]

let opams =
  let opam no_lint name =
    Pkg.opam_file ~lint_deps_excluding:(Some no_lint) ~install:false name
  in
  [
    opam
      ["lwt"; "cstruct"; "mirage-flow"; "mirage-time"; "result";
       "alcotest"; "mirage-clock"; "mirage-flow-lwt"] "opam";
    opam ["alcotest"; "mirage-flow-lwt"; "result"] "mirage-flow-lwt.opam";
    opam ["mirage-time"; "result"; "mirage-clock"] "mirage-flow-unix.opam";
  ]

let () =
  Pkg.describe ~metas ~opams "mirage-flow" @@ fun c ->
  match Conf.pkg_name c with
  | "mirage-flow" ->
    Ok [ Pkg.lib "pkg/META";
         Pkg.mllib "src/mirage-flow.mllib" ]
  | "mirage-flow-lwt" ->
    Ok [ Pkg.lib "pkg/META.lwt" ~dst:"META";
         Pkg.mllib "lwt/mirage-flow-lwt.mllib" ]
  | "mirage-flow-unix" ->
    Ok [ Pkg.lib "pkg/META.unix" ~dst:"META";
         Pkg.mllib "unix/mirage-flow-unix.mllib";
         Pkg.test "test/test" ]
  | other ->
    R.error_msgf "unknown package name: %s" other
