all: lib test

lib: pbkdf.ml
	ocamlbuild pbkdf.cmx -pkg cstruct

test: pbkdftest.ml
	ocamlbuild pbkdftest.native -pkg cstruct -pkg nocrypto -pkg alcotest

clean:
	rm -rf _build _tests *.byte *.native
