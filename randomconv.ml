
type int8 = int

let int8 g = Cstruct.get_uint8 (g 1) 0

type int16 = int

let int16 g = Cstruct.LE.get_uint16 (g 2) 0

let int32 g = Cstruct.LE.get_uint32 (g 4) 0

let int64 g = Cstruct.LE.get_uint64 (g 8) 0

let bitmask n =
  let rec go c = function
    | 0 -> c
    | n -> go (c lsl 1) (n lsr 1)
  in
  go 1 n - 1

let rec int ?(bound = max_int) g =
  if bound <= 0 then invalid_arg "bound smaller or equal 0 not supported" ;
  if bound = 1 then 0
  else
    let r =
      if bound <= 256 then
        int8 g
      else if bound <= 65536 then
        int16 g
      else
        match Sys.word_size with
        | 32 -> Int32.to_int (int32 g)
        | 64 -> Int64.to_int (int64 g)
        | _ -> invalid_arg "unknown word size"
    in
    let r = r land bitmask (pred bound) in
    if r < bound then r else int ~bound g

let float ?(bound = 1.) g =
  if bound <= 0. then invalid_arg "bound smaller or equal 0 not supported" ;
  let scale = float_of_int max_int
  and r1 = int g
  and r2 = int g
  in
  bound *. ((float_of_int r1 /. scale +. float_of_int r2) /. scale)
