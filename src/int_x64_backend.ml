type t = int

let zero = 0
let one = 1
let minus_one = (-1)
let neg x = (-x)
let add a b = a + b
let sub a b = a - b
let mul a b = a * b
let div a b = a / b
let rem a b = a mod b
let succ x = x + 1
let pred x = x - 1
let abs x = Pervasives.abs x
let max_int = Int32.(to_int max_int)
let min_int = Int32.(to_int min_int)
let logand a b = a land b
let logor a b = a lor b
let logxor a b = a lxor b
let lognot x = lnot (x land 0xffffffff)
let shift_left a n = a lsl n
let shift_right a n = a lsr n
let shift_right_logical a n = a asr n
external of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"
let of_int32 = Int32.to_int
let to_int32 = Int32.of_int
let of_float = int_of_float
let to_float = float_of_int
let of_string = int_of_string
let of_string_opt x = try Some (of_string x) with Failure _ -> None
let to_string = string_of_int
let compare : int -> int -> int = fun a b -> a - b
let equal : int -> int -> bool = fun a b -> a = b

let pp ppf (x:t) = Format.fprintf ppf "%d" x

module Infix = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( % ) = rem
  let ( / ) = div

  let ( && ) = logand
  let ( || ) = logor
  let ( >> ) = shift_right
  let ( << ) = shift_left
end
