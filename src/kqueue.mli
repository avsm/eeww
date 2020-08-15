module Timespec : sig
  type t

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

  val all : t list
end

type t

val kqueue : unit -> t
