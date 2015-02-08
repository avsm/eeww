#!/bin/sh -ex
# Generate the mime_types.ml file

ocaml str.cma generate_mime_types.ml mime.types > mime_types.ml
