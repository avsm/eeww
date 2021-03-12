ocaml-dispatch -- bindings to Apple's Grand Central Dispatch
-----------------------------------------------------------

*Status: Experimental & WIP*
 
These are OCaml bindings for Apple's Grand Central Dispatch (GCD) library.

This project is heavily influenced by [ocaml-uring](https://github.com/ocaml-multicore/ocaml-uring/). In particular the copying tests (framework and `lwtcp`) are taken directly from that repository. (TODO: attribute this properly in the code).

Known problems:
 - I'm pretty sure the calls in the stubs to `caml_alloc_custom_mem` need to register a finaliser or something to free that memory. 
 - This may not be the best implementation -- documentation for GCD is a little thin on the ground, but for example instead of calling lots of small `dispatch_io_read` operations for a single large file, I think you just need to call it once and the handler is called multiple times with chunks of data (maybe based on the *high water mark*?).