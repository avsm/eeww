(** Unboxed option using a unique block to identify {!none}. *)

type !'a t

val none : 'a t
val some : 'a -> 'a t
val is_none : 'a t -> bool
val is_some : 'a t -> bool
val get_or_retry : 'a t -> 'a
val get_unsafe : 'a t -> 'a
val to_option : 'a t -> 'a option
