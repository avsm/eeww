type t

val zero: t
val one: t
val minus_one: t
val neg: t -> t
val add: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t
val div: t -> t -> t
val rem: t -> t -> t
val succ: t -> t
val pred: t -> t
val abs: t -> t
val max_int: t
val min_int: t
val logand: t -> t -> t
val logor: t -> t -> t
val logxor: t -> t -> t
val lognot: t -> t
val shift_left: t -> int -> t
val shift_right: t -> int -> t
val shift_right_logical: t -> int -> t
val of_int: int -> t
val to_int: t -> int
val of_float: float -> t
val to_float: t -> float
val of_string: string -> t
val of_string_opt: string -> t option
val to_string: t -> string
val compare: t -> t -> int
val equal: t -> t -> bool
val pp: Format.formatter -> t -> unit

module Infix: sig
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val ( * ): t -> t -> t
  val ( % ): t -> t -> t
  val ( / ): t -> t -> t
end
