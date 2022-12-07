```
git subtree add --prefix boot/ocaml https://github.com/ocaml/ocaml trunk
git rm --cached boot/ocaml/flexdll
git commit -a -m 'ocaml: remove submodules'
git subtree add --prefix boot/dune https://github.com/ocaml/dune 3.6 
git subtree add --prefix lib/re https://github.com/ocaml/ocaml-re master 
git subtree add --prefix lib/cmdliner https://erratique.ch/repos/cmdliner.git master 
git subtree add --prefix lib/uutf https://github.com/dune-universe/uutf.git duniverse-v1.0.3
git subtree add --prefix lib/fmt https://github.com/dune-universe/fmt dune-universe-v0.9.0
git subtree add --prefix lib/ounit https://github.com/gildor478/ounit.git master 
git subtree add --prefix lib/cstruct https://github.com/mirage/ocaml-cstruct.git main 
git subtree add --prefix lib/magic-mime https://github.com/mirage/ocaml-magic-mime.git master 
git subtree add --prefix lib/alcotest https://github.com/mirage/alcotest.git main 
git subtree add --prefix lib/eio https://github.com/ocaml-multicore/eio.git main 
git subtree add --prefix lib/cohttp https://github.com/mirage/ocaml-cohttp.git master 
git subtree add --prefix lib/fpath https://github.com/dune-universe/fpath duniverse-v0.7.3 
git subtree add --prefix lib/uri https://github.com/mirage/ocaml-uri.git master 
git subtree add --prefix lib/optint https://github.com/mirage/optint.git master 
git subtree add --prefix lib/mtime https://github.com/dune-universe/mtime.git duniverse-v1.4.0 
git subtree add --prefix lib/bigstringaf https://github.com/inhabitedtype/bigstringaf.git master 
git subtree add --prefix lib/lwt-dllist https://github.com/mirage/lwt-dllist.git master 
git subtree add --prefix lib/hmap https://github.com/dune-universe/hmap.git duniverse-v0.8.1 
git subtree add --prefix lib/logs https://github.com/dune-universe/logs.git duniverse-v0.7.0 
git subtree add --prefix lib/uring https://github.com/ocaml-multicore/ocaml-uring main 
git subtree add --prefix lib/psq https://github.com/pqwy/psq.git master 
git subtree add --prefix lib/angstrom https://github.com/inhabitedtype/angstrom.git master 
git subtree add --prefix lib/stringext https://github.com/rgrinberg/stringext master 
git subtree add --prefix lib/astring https://github.com/dune-universe/astring.git duniverse-v0.8.5 
git subtree add --prefix lib/meio https://github.com/patricoferris/meio.git main 
git rm --cached lib/meio/eio
git commit -a -m 'meio: remove submodules'
git subtree add --prefix boot/dune https://github.com/ocaml/dune 3.6 
git subtree add --prefix lib/hdr_histogram https://github.com/ocaml-multicore/hdr_histogram_ocaml.git main 
git subtree add --prefix lib/lwd https://github.com/let-def/lwd.git master 
git subtree add --prefix lib/ptime https://github.com/dune-universe/ptime.git dune-universe-v1.0.0 
git subtree add --prefix lib/notty https://github.com/pqwy/notty.git master 
git subtree add --prefix lib/ctypes https://github.com/avsm/ocaml-ctypes.git dune-port 
git subtree add --prefix lib/integers https://github.com/yallop/ocaml-integers.git master 
git subtree add --prefix lib/bigarray-compat https://github.com/mirage/bigarray-compat.git master 
```
