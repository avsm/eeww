OCAMLBUILD=ocamlbuild -tag debug -classic-display -use-ocamlfind
OCAMLDOCFLAGS=-docflags -colorize-code,-charset,utf-8
BUILDDIR=_build
DOCDIR=doc/api.docdir

all: lib test docs

lib: pbkdf.ml pbkdf.mli
	${OCAMLBUILD} pbkdf.cmx

test: pbkdf_tests.ml pbkdf.ml pbkdf.mli
	${OCAMLBUILD} pbkdf_tests.native

docs: pbkdf.mli
	${OCAMLBUILD} -no-links ${OCAMLDOCFLAGS} doc/api.docdir/index.html
	cp doc/style.css ${BUILDDIR}/${DOCDIR}/style.css

clean:
	${OCAMLBUILD} -clean
	rm -rf _tests
