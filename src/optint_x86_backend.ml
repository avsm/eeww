include Int32

external of_int32 : int32 -> t = "%identity"
external to_int32 : t -> int32 = "%identity"

let to_int64 = Int64.of_int32
let of_int64 = Int64.to_int32

let pp ppf (x:t) = Format.fprintf ppf "%ld" x

let bit_sign = 0x40000000l
let without_bit_sign (x:int) = if x >= 0 then x else x land (lnot 0x40000000)

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let to_int x =
  let max_int = of_int Stdlib.max_int in
  if compare zero x <= 0 && compare x max_int <= 0
  then to_int x (* XXX(dinosaure): can fit into 31 bits. *)
  else invalid_arg "Optint.to_int: %ld can not fit into a 31 bits integer" x

let of_int x =
  if x < 0
  then
    let x = without_bit_sign x in
    logor bit_sign (of_int x) (* XXX(dinosaure): keep sign bit into the same position. *)
  else of_int x

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
