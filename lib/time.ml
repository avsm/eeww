type t
(** Abstract type for GCD's notion of time *)

external dispatch_now : unit -> t = "ocaml_dispatch_now"
external dispatch_forever : unit -> t = "ocaml_dispatch_forever"

let now = dispatch_now
let forever = dispatch_forever
