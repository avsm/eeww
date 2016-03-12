all: lib test

lib: pbkdf.ml
	ocamlbuild pbkdf.cmx -pkg cstruct -pkg nocrypto

test: pbkdf_tests.ml
	ocamlbuild pbkdf_tests.native -pkg cstruct -pkg nocrypto -pkg alcotest

clean:
	rm -rf _build _tests *.byte *.native
