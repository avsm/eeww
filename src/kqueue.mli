module Timespec : sig
  type t [@@deriving sexp]

  val sec : t -> int64

  val nsec : t -> int64

  val make : sec:int64 -> nsec:int64 -> t
end

module Flag : sig
  type t =
    | Add
    | Enable
    | Disable
    | Dispatch
    | Delete
    | Receipt
    | Oneshot
    | Clear
    | EOF
    | Error
  [@@deriving sexp]
end

type t

val kqueue : unit -> t
