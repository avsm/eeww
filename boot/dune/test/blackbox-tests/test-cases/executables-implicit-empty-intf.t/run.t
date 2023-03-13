Executables with no corresponding `.mli` file will have one generated for them
by Dune:

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > (executables_implicit_empty_intf true)
  > EOF

We rewrite the warning to ensure tests behave the same on 4.11 and 4.12
  $ dune build ./bin/executable.exe 2>&1 |
  > sed -e 's,(warn.*32.*),(warning 32),g'
  File "bin/executable.ml", line 1, characters 4-10:
  1 | let unused = Dependency.a
          ^^^^^^
  Error (warning 32): unused value unused.

  $ test ! -f _build/default/bin/dependency.mli
  $ cat _build/default/bin/executable.mli
  (* Auto-generated by Dune *)

as will test binaries:

  $ dune runtest 2>&1 |
  > sed -e 's,(warn.*32.*),(warning 32),g'
  File "test/test.ml", line 1, characters 4-10:
  1 | let unused = 1
          ^^^^^^
  Error (warning 32): unused value unused.

  $ cat _build/default/test/test.mli
  (* Auto-generated by Dune *)

If an executable already has an interface, it is preserved:

  $ dune clean
  $ dune build ./bin_with_intf/executable.exe
  $ cat _build/default/bin_with_intf/executable.mli
  val a : int

Generation of empty `.mli` files is disabled by default prior to lang 3.0:

  $ dune clean
  $ echo >dune-project "(lang dune 2.9)"
  $ dune build ./bin/executable.exe
  $ dune runtest
  $ test ! -f _build/default/bin/executable.mli
  $ test ! -f _build/default/test/test.mli

