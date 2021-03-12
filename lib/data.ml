type t

type buff =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external dispatch_data_create : buff -> t = "ocaml_dispatch_data_create"
external dispatch_data_empty : unit -> t = "ocaml_dispatch_data_empty"
external dispatch_data_size : t -> int = "ocaml_dispatch_data_size"

external dispatch_data_apply : (t -> 'b) -> t -> 'b
  = "ocaml_dispatch_data_apply"

let create = dispatch_data_create
let empty = dispatch_data_empty
let size = dispatch_data_size
let apply f t = dispatch_data_apply f t
