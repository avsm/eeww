#!/bin/sh -e
# Output useful packages + URLs to vendor

OCAML_VERSION=5.0.0
PACKAGES="eio,cohttp-eio"

export OPAMROOT=`pwd`/_obj/_opam
rm -rf "${OPAMROOT}"
mkdir -p "${OPAMROOT}"
export OPAMYES=1
export PATH=`pwd`/_obj/bin:$PATH
opam init -yan --bypass-checks --bare --quiet
opam switch create comp --empty --quiet
opam pin add -n ocaml-system.${OCAML_VERSION} `pwd`/boot/opam-repo --quiet
opam install ocaml-system.${OCAML_VERSION} --quiet
opam remote add alpha https://github.com/kit-ty-kate/opam-alpha-repository.git --quiet
opam remote add duni https://github.com/dune-universe/opam-overlays.git --quiet
opam list --with-test --resolve="$PACKAGES" -sS --columns name,dev-repo: | sed -e 's,"git+https://\(.*\)"$,https://\1,g'| grep https://
