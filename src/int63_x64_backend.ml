type t = int

let zero = 0
let one = 1
let minus_one = -1
let neg x = -x
let add a b = a + b
let sub a b = a - b
let mul a b = a * b
let div a b = a / b
let rem a b = a mod b
let succ x = succ x
let pred x = pred x
let logand a b = a land b
let logor a b = a lor b
let logxor a b = a lxor b
let lognot x = lnot x
let shift_left a n = a lsl n
let shift_right a n = a asr n
let shift_right_logical a n = a lsr n
let abs x = abs x
let max_int = max_int
let min_int = min_int

external of_int : t -> t = "%identity"
external to_int : t -> t = "%identity"

let to_int32 = Stdlib.Int32.of_int
let of_int32 = Stdlib.Int32.to_int
let to_int64 = Stdlib.Int64.of_int
let of_int64 = Stdlib.Int64.to_int
let of_float x = int_of_float x
let to_float x = float_of_int x
let of_string x = int_of_string x
let of_string_opt x = try Some (of_string x) with Failure _ -> None
let to_string x = string_of_int x
let equal : int -> int -> bool = fun a b -> a = b
let compare : int -> int -> int = fun a b -> compare a b
let pp = Format.pp_print_int

module Infix = struct
  let ( + ) a b = add a b
  let ( - ) a b = sub a b
  let ( * ) a b = mul a b
  let ( % ) a b = rem a b
  let ( / ) a b = div a b
  let ( && ) a b = logand a b
  let ( || ) a b = logor a b
  let ( >> ) a b = shift_right a b
  let ( << ) a b = shift_left a b
end
