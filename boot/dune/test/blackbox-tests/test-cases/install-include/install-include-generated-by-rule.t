Including a file in the install stanza which is generated by a rule

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name hello))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (public_name hello))
  > 
  > (rule
  >  (target foo.sexp)
  >  (action
  >   (with-stdout-to foo.sexp (echo "(a.txt)"))))
  > 
  > (install
  >  (files (include foo.sexp))
  >  (section share))
  > EOF

  $ cat >hello.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ touch a.txt

  $ dune build @install

  $ cat _build/default/hello.install
  lib: [
    "_build/install/default/lib/hello/META"
    "_build/install/default/lib/hello/dune-package"
  ]
  bin: [
    "_build/install/default/bin/hello"
  ]
  share: [
    "_build/install/default/share/hello/a.txt"
  ]
