type t
type typ = Stream | Random
type err = int
type handler = err:err -> finished:bool -> Data.t -> unit

external dispatch_io_create : typ -> Unix.file_descr -> Queue.t -> t
  = "ocaml_dispatch_io_create"

external dispatch_with_read :
  t -> int -> int -> Queue.t -> (int -> bool -> Data.t -> unit) -> unit
  = "ocaml_dispatch_with_read"

external dispatch_with_write :
  t -> int -> Data.t -> Queue.t -> (int -> bool -> Data.t -> unit) -> unit
  = "ocaml_dispatch_with_write"

external dispatch_write : Queue.t -> t -> int -> Group.t -> Data.t -> unit
  = "ocaml_dispatch_write"

external dispatch_set_high_water : t -> int -> unit
  = "ocaml_dispatch_set_high_water"

external dispatch_set_low_water : t -> int -> unit
  = "ocaml_dispatch_set_low_water"

let create typ fd q = dispatch_io_create typ fd q

let with_read ~f ~off ~length ~queue channel =
  let f' err finished data = f ~err ~finished data in
  dispatch_with_read channel off length queue f'

let with_write ~f ~off ~data ~queue channel =
  let f' err finished data = f ~err ~finished data in
  dispatch_with_write channel off data queue f'

let write queue channel offset group data =
  let _ = dispatch_write queue channel offset group data in
  ()

let set_high_water = dispatch_set_high_water
let set_low_water = dispatch_set_low_water
