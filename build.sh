#!/bin/sh
# This script is only used for developement. It is removed by the
# distribution process.

set -e

OCAMLBUILD=${OCAMLBUILD:="ocamlbuild -tag debug -classic-display \
                          -use-ocamlfind" }
OCAMLDOCFLAGS=${OCAMLDOCFLAGS:="-docflags -colorize-code,-charset,utf-8"}
BUILDDIR=${BUILDDIR:="_build"}

action ()
{
    case $1 in
        default) action lib ;;
        lib) $OCAMLBUILD hkdf.cmx ;;
        test) action lib ; $OCAMLBUILD rfctests.native ;;
        doc) shift
             $OCAMLBUILD -no-links $OCAMLDOCFLAGS doc/api.docdir/index.html
             cp doc/style.css $BUILDDIR/$DOCDIRFILE/style.css ;;
        clean) $OCAMLBUILD -clean ; rm -rf _tests ;;
        *) $OCAMLBUILD $* ;;
    esac
}

if [ $# -eq 0 ];
then action default ;
else action $*; fi
