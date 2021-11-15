type t

module Flag : sig
  type t

  val pp : Format.formatter -> t -> unit
  val ( + ) : t -> t -> t
  val is_subset : t -> of_:t -> bool
  val ev_add : t
  val ev_enable : t
  val ev_disable : t
  val ev_delete : t
  val ev_oneshot : t
  val ev_clear : t
  val ev_eof : t
  val ev_error : t
end

type event =
  [ `Read
  | `Write
  ]

module Timeout : sig
  type t

  val never : t
  val immediate : t
  val of_ms : int -> t
end

val pp_event : Format.formatter -> event -> unit
val kqueue : changelist_size:int -> t
val add : t -> Unix.file_descr -> [ event | `Read_write ] -> unit
val wait : t -> Timeout.t -> [ `Ok | `Timeout ]
val iter_ready : t -> f:(Unix.file_descr -> Flag.t -> event -> unit) -> unit
val close : t -> unit
