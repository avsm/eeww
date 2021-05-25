type t
(** The type of IO Channels *)

type typ =
  | Stream
  | Random
      (** The underlying type of the channel -- with [Random] you can use the
          file offset to read *)

val create : typ -> Unix.file_descr -> Queue.t -> t
(** Create a new channel of a particular [typ] from a [Unix.file_descr] *)

(* val read : Queue.t -> t -> int -> int -> Group.t -> Data.t -> unit *)

type err = int
(** Error type *)

type handler = err:err -> finished:bool -> Data.t -> unit
(** The type of IO handler *)

val with_read : f:handler -> off:err -> length:err -> queue:Queue.t -> t -> unit
(** [with_read ~f ~off ~length ~queue channel] performs an asynchronous read of
    [channel] using the [handler] to deal with data returned which is scheduled
    onto [queue] *)

val with_write :
  f:handler -> off:int -> data:Data.t -> queue:Queue.t -> t -> unit
(** [with_read ~f ~off ~data ~queue channel] performs an asynchronous write to
    [channel] using the [handler] to deal with data returned which is scheduled
    onto [queue] *)

val write : Queue.t -> t -> int -> Group.t -> Data.t -> unit
val set_high_water : t -> int -> unit
val set_low_water : t -> int -> unit
