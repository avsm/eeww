type t
type typ = Stream | Random  (** GCD IO channels *)

val create : typ -> Unix.file_descr -> Queue.t -> t
val read : Queue.t -> t -> int -> int -> Group.t -> Data.t -> unit
val write : Queue.t -> t -> int -> Group.t -> Data.t -> unit
val set_high_water : t -> int -> unit
val set_low_water : t -> int -> unit
