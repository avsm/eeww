type t
(** The type for dispatch blocks (essentially thunks) *)

val create : (unit -> unit) -> t
(** [create thunk] makes a new dispatch block *)

val exec : t -> unit
(** [exec t] executes the block *)
