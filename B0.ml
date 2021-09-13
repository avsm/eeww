open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let uucp = B0_ocaml.libname "uucp"

let uucd = B0_ocaml.libname "uucd"
let uunf = B0_ocaml.libname "uunf"
let uutf = B0_ocaml.libname "uutf"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let uucp_lib =
  let srcs = Fpath.[ `Dir (v "src") ] in
  let requires = [] in
  B0_ocaml.lib uucp ~doc:"The uucp library" ~srcs ~requires

(* Tools *)

let ucharinfo =
  let srcs = Fpath.[`File (v "test/ucharinfo.ml")] in
  let requires = [cmdliner; uutf; uunf; uucp] in
  B0_ocaml.exe "uncharinfo" ~doc:"The ucharinfo tool" ~srcs ~requires

(* Tests *)

let test ?(requires = [uucp]) name ~src ~doc =
  let srcs = Fpath.[`File (v src)] in
  let meta = B0_meta.(empty |> tag test) in
  B0_ocaml.exe name ~doc ~srcs ~meta ~requires

let test' =
  test "test" ~requires:[uucd; uucp] ~src:"test/test.ml" ~doc:"Test"

let perf =
  test "perf" ~requires:[uucp] ~src:"test/perf.ml" ~doc:"Test performance"

let link_test =
  test "link_test" ~requires:[uucp] ~src:"test/link_test.ml" ~doc:"Link test"

let examples =
  test "examples" ~requires:[uucp; uutf; uunf]
    ~src:"test/examples.ml" ~doc:"Examples"

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The uucp programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/uucp"
    |> add online_doc "https://erratique.ch/software/uucp/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/uucp.git"
    |> add issues "https://github.com/dbuenzli/uucp/issues"
    |> add description_tags
      ["unicode"; "text"; "character"; "org:erratique"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
         "--with-uutf" "%{uutf:installed}%"
         "--with-uunf" "%{uunf:installed}%"
         "--with-cmdliner" "%{cmdliner:installed}%" ]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depopts [ "uutf", ""; "uunf", ""; "cmdliner", ""]
    |> add B0_opam.Meta.conflicts [ "uutf", {|< "1.0.1"|};
                                    "cmdliner", {|< "1.0.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.03.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uucd", {|with-test|};
        "uunf", {|with-test|};
        "uutf", {|with-test|} ]
  in
  B0_pack.v "default" ~doc:"uucd package" ~meta ~locked:true @@
  B0_unit.list ()
