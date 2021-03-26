type t
type typ = Stream | Random

external dispatch_io_create : typ -> Unix.file_descr -> Queue.t -> t
  = "ocaml_dispatch_io_create"

(* external dispatch_read : Queue.t -> t -> int -> int -> Group.t -> Data.t -> unit
  = "ocaml_dispatch_read_bytecode" "ocaml_dispatch_read" *)

external dispatch_with_read :
  Queue.t -> t -> Group.t -> (Data.t -> unit) -> (unit -> unit) -> unit
  = "ocaml_dispatch_with_read"

external dispatch_write : Queue.t -> t -> int -> Group.t -> Data.t -> unit
  = "ocaml_dispatch_write"

external dispatch_set_high_water : t -> int -> unit
  = "ocaml_dispatch_set_high_water"

external dispatch_set_low_water : t -> int -> unit
  = "ocaml_dispatch_set_low_water"

let create typ fd q = dispatch_io_create typ fd q

(* let read queue channel length offset group data =
  let _ = dispatch_read queue channel length offset group data in
  () *)

let with_read ~f ~err queue channel group =
  dispatch_with_read queue channel group f err

let write queue channel offset group data =
  let _ = dispatch_write queue channel offset group data in
  ()

let set_high_water = dispatch_set_high_water
let set_low_water = dispatch_set_low_water
