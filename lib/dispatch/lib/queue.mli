type t
type typ = Serial | Concurrent

val main : unit -> t

module Qos : sig
  type t = Interactive | User_initiated | Utility | Background
end

val global : Qos.t -> t
val create : ?typ:typ -> unit -> t
