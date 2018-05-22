include Int32

let pp ppf = Format.fprintf ppf "%ld"

module Infix = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( % ) = rem
  let ( / ) = div
end
