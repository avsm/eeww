type t = int

let zero = 0
let one = 1
let minus_one = (-1)
let neg x = (-x)
let add a b = a + b
let sub a b = a - b
let mul a b = a * b

let _unsigned_compare n m =
  let open Nativeint in
  compare (sub n min_int) (sub m min_int)

let _unsigned_div n d =
  let open Nativeint in
  if d < zero then
    if _unsigned_compare n d < 0 then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if _unsigned_compare r d >= 0 then succ q else q

let div a b = Nativeint.to_int (_unsigned_div (Nativeint.of_int a) (Nativeint.of_int b))
let rem a b = a mod b
let succ x = x + 1
let pred x = x - 1
let abs x =
  let mask = x asr Sys.int_size in (* extract sign: -1 if signed, 0 if not signed *)
  (x + mask) lxor mask
let max_int = Int32.(to_int max_int)
let min_int = Int32.(to_int min_int)
let logand a b = a land b
let logor a b = a lor b
let logxor a b = a lxor b
let lognot x = lnot x
let shift_left a n = a lsl n
let shift_right a n = a asr n
let shift_right_logical a n = a lsr n
external of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"
let to_int64 = Stdlib.Int64.of_int
let of_int64 = Stdlib.Int64.to_int
let of_float x = int_of_float x
let to_float x = (* allocation *) float_of_int x
let of_string x = int_of_string x
let of_string_opt x = try (* allocation *) Some (of_string x) with Failure _ -> None
let to_string x = string_of_int x
let compare : int -> int -> int = fun a b -> a - b
let equal : int -> int -> bool = fun a b -> a = b

let bit_sign = 0x80000000
let without_bit_sign (x:int32) = if x >= 0l then x else Int32.logand x (Int32.lognot 0x80000000l)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let to_int32 x =
  let truncated = x land 0xffffffff in
  if x <> truncated
  then invalid_arg "Optint.to_int32: %d can not fit into a 32 bits integer" x
  else Int32.of_int truncated

let of_int32 x =
  if x < 0l
  then
    let x = without_bit_sign x in
    (Int32.to_int x) lor bit_sign (* XXX(dinosaure): keep bit sign into the same position. *)
  else Int32.to_int x

let pp ppf (x:t) = Format.fprintf ppf "%d" x

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
