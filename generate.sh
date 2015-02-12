#!/bin/sh -ex
# Generate the mime_types.ml file

cat mime.types more-mime.types | ocaml str.cma generate_mime_types.ml > mime_types.ml
