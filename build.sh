#!/bin/sh
# This script is only used for developement. It is removed by the
# distribution process.

set -e

OCAMLBUILD=${OCAMLBUILD:="ocamlbuild -tag debug -classic-display \
                          -use-ocamlfind" }

action ()
{
    case $1 in
        default) action lib ;;
        lib) $OCAMLBUILD hkdf.cmx rfctests.native ;;
        clean)   $OCAMLBUILD -clean ; rm -rf _tests ;;
        *)       $OCAMLBUILD $* ;;
    esac
}

if [ $# -eq 0 ];
then action default ;
else action $*; fi
