include Int32

external of_int32 : int32 -> t = "%identity"
external to_int32 : t -> int32 = "%identity"

let pp ppf (x:t) = Format.fprintf ppf "%ld" x

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
