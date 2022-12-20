type t

external dispatch_block_create : (unit -> unit) -> t
  = "ocaml_dispatch_block_create"

external dispatch_block_destroy : t -> unit = "ocaml_dispatch_block_destroy"
external dispatch_block_exec : t -> unit = "ocaml_dispatch_block_exec"

let create thunk =
  let b = dispatch_block_create thunk in
  Gc.finalise dispatch_block_destroy b;
  b

let exec = dispatch_block_exec
