
let int32 g = Cstruct.LE.get_uint32 (g 4) 0

let int64 g = Cstruct.LE.get_uint64 (g 8) 0

let int ?(bound = max_int) g =
  if bound <= 1 then invalid_arg "bound smaller or equal 1 not supported" ;
  let r =
    if bound < 256 then
      Cstruct.get_uint8 (g 1) 0
    else if bound < 65536 then
      Cstruct.LE.get_uint16 (g 2) 0
    else
      match Sys.word_size with
      | 32 -> Int32.to_int (int32 g) mod bound
      | 64 -> Int64.to_int (int64 g) mod bound
      | _ -> invalid_arg "unknown word size"
  in
  if r < 0 then r + bound else r

let float ?(bound = 1.) g =
  if bound <= 0. then invalid_arg "bound smaller or equal 0 not supported" ;
  let scale = float_of_int max_int
  and r1 = int g
  and r2 = int g
  in
  bound *. ((float_of_int r1 /. scale +. float_of_int r2) /. scale)
