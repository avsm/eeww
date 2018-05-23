include Int32

external of_int32 : int32 -> t = "%identity"
external to_int32 : t -> int32 = "%identity"

let pp ppf (x:t) = Format.fprintf ppf "%ld" x

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
