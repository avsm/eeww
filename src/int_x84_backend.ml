include Int32

let pp ppf (x:t) = Format.fprintf ppf "%ld" x

module Infix = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( % ) = rem
  let ( / ) = div
end
