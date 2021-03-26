type t
type typ = Serial | Concurrent

(* val main : unit -> t
val global : unit -> t *)
val create : ?typ:typ -> unit -> t
