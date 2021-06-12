type t
(** Dispatch data objects *)

type buff =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** A buffer to create a data object from *)

val create : buff -> t
(** [create buff] creates a new data object from [buff] *)

val empty : unit -> t
(** [empty ()] returns an empty data object *)

val size : t -> int
(** [size data] returns the size of the data object *)

val apply : (t -> unit) -> t -> unit
(** [apply f t] applies [f] to [t] using the
    {{:https://developer.apple.com/documentation/dispatch/1452852-dispatch_data_apply?language=objc}
    dispatch_data_apply} function. Crucially, if the underlying memory of the
    data object is non-contiguous then the function is applied to each
    individual data object that makes up the larger one. *)

val concat : t -> t -> t
(** [concat t1 t2] creates a new [t3] by concating [t1] and [t2] -- note the
    data of [t3] is non-contiguous so this operation should be fast *)

val to_buff : offset:int -> int -> t -> buff
(** [to_buff t] returns the data as a {!buff} *)

val sub : int -> int -> t -> t
(** [sub off len t] returns a new data object consisting of a portion of
    another's memory region *)
