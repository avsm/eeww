open Core_kernel

type t [@@deriving sexp_of]

module Flag : sig
  include Flags.S

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

val kqueue : changelist_size:int -> t
val add : t -> Caml_unix.file_descr -> event -> unit
val wait : t -> Time_ns.Span.t -> [ `Ok | `Timeout ]
val iter_ready : t -> f:(Caml_unix.file_descr -> Flag.t -> event -> unit) -> unit
val close : t -> unit
