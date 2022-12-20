type t
type state = Ready | Pending

external dispatch_group_create : unit -> t = "ocaml_dispatch_group_create"

external dispatch_group_wait : t -> Time.t -> state
  = "ocaml_dispatch_group_wait"

external dispatch_group_enter : t -> unit = "ocaml_dispatch_group_enter"
external dispatch_group_leave : t -> unit = "ocaml_dispatch_group_leave"

let create = dispatch_group_create
let wait = dispatch_group_wait
let enter = dispatch_group_enter
let leave = dispatch_group_leave
