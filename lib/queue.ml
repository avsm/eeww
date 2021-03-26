type t
type typ = Serial | Concurrent

(* external dispatch_main : unit -> t = "ocaml_dispatch_get_main_queue" *)
(* external dispatch_global : unit -> t = "ocaml_dispatch_get_global_queue" *)
external dispatch_create : typ -> t = "ocaml_dispatch_queue_create"
external dispatch_finalise : t -> unit = "ocaml_dispath_queue_finalise"

(* let main = dispatch_main
let global = dispatch_global *)

let create ?(typ = Serial) () =
  let t = dispatch_create typ in
  Gc.finalise dispatch_finalise t;
  t
