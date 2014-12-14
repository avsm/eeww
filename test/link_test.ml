(*
  Compile with:
  ocamlfind ocamlopt -package uucp -linkpkg -o link_test.native link_test.ml
*)

let () = ignore (Uucp.Age.age 0x1F42B)
