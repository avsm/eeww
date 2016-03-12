OCAMLBUILD=ocamlbuild -tag debug -classic-display -use-ocamlfind
OCAMLDOCFLAGS=-docflags -colorize-code,-charset,utf-8
BUILDDIR=_build

all: lib test

lib: pbkdf.ml pbkdf.mli
	${OCAMLBUILD} pbkdf.cmx

test: pbkdf_tests.ml pbkdf.ml pbkdf.mli
	${OCAMLBUILD} pbkdf_tests.native

clean:
	${OCAMLBUILD} -clean
	rm -rf _tests
