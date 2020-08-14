module Timespec : sig
  type t [@@deriving sexp]

  val sec : t -> int64

  val nsec : t -> int64

  val make : sec:int64 -> nsec:int64 -> t
end

type t

val kqueue : unit -> t
