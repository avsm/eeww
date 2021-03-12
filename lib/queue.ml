type t
type typ = Serial | Concurrent

external dispatch_main : unit -> t = "ocaml_dispatch_get_main_queue"
external dispatch_global : unit -> t = "ocaml_dispatch_get_global_queue"
external dispatch_create : typ -> t = "ocaml_dispatch_queue_create"

let main = dispatch_main
let global = dispatch_global
let create ?(typ = Serial) () = dispatch_create typ
