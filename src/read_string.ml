let sign8 v =
  (v lsl ( Sys.word_size - 9 )) asr ( Sys.word_size - 9 )

let sign16 v =
  (v lsl ( Sys.word_size - 17 )) asr ( Sys.word_size - 17 )

let get_char (s:string) off =
  String.get s off
let get_uint8 (s:string) off =
  Char.code (String.get s off)
let get_int8 s off =
  sign8 (get_uint8 s off)
let set_char (s:string) off v =
  String.set s off v
let set_int8 (s:string) off v =
  set_char s off (Char.chr v)

let unsafe_get_char (s:string) off =
  String.unsafe_get s off
let unsafe_get_uint8 (s:string) off =
  Char.code (String.unsafe_get s off)
let unsafe_get_int8 s off =
  sign8 (unsafe_get_uint8 s off)
let unsafe_set_char (s:string) off v =
  String.unsafe_set s off v
let unsafe_set_int8 (s:string) off v =
  unsafe_set_char s off (Char.chr v)

#if ocaml_version >= (4, 1)

external unsafe_get_16 : string -> int -> int = "%caml_string_get16u"
external unsafe_get_32 : string -> int -> int32 = "%caml_string_get32u"
external unsafe_get_64 : string -> int -> int64 = "%caml_string_get64u"

external unsafe_set_16 : string -> int -> int -> unit = "%caml_string_set16u"
external unsafe_set_32 : string -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_set_64 : string -> int -> int64 -> unit = "%caml_string_set64u"

external get_16 : string -> int -> int = "%caml_string_get16"
external get_32 : string -> int -> int32 = "%caml_string_get32"
external get_64 : string -> int -> int64 = "%caml_string_get64"

external set_16 : string -> int -> int -> unit = "%caml_string_set16"
external set_32 : string -> int -> int32 -> unit = "%caml_string_set32"
external set_64 : string -> int -> int64 -> unit = "%caml_string_set64"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external swapnative : nativeint -> nativeint = "%bswap_native"

module BE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

  let get_uint16 s off =
    let v = get_16 s off in
    if Sys.big_endian
    then v
    else swap16 v

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let v = get_32 s off in
    if Sys.big_endian
    then v
    else swap32 v

  let get_int64 s off =
    let v = get_64 s off in
    if Sys.big_endian
    then v
    else swap64 v

  let set_int16 s off v =
    let v =
      if Sys.big_endian
      then v
      else swap16 v in
    set_16 s off v

  let set_int32 s off v =
    let v =
      if Sys.big_endian
      then v
      else swap32 v in
    set_32 s off v

  let set_int64 s off v =
    let v =
      if Sys.big_endian
      then v
      else swap64 v in
    set_64 s off v
end

module BE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

  let get_uint16 s off =
    let v = unsafe_get_16 s off in
    if Sys.big_endian
    then v
    else swap16 v

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let v = unsafe_get_32 s off in
    if Sys.big_endian
    then v
    else swap32 v

  let get_int64 s off =
    let v = unsafe_get_64 s off in
    if Sys.big_endian
    then v
    else swap64 v

  let set_int16 s off v =
    let v =
      if Sys.big_endian
      then v
      else swap16 v in
    set_16 s off v

  let set_int32 s off v =
    let v =
      if Sys.big_endian
      then v
      else swap32 v in
    set_32 s off v

  let set_int64 s off v =
    let v =
      if Sys.big_endian
      then v
      else swap64 v in
    set_64 s off v
end

module LE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

  let get_uint16 s off =
    let v = get_16 s off in
    if Sys.big_endian
    then swap16 v
    else v

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let v = get_32 s off in
    if Sys.big_endian
    then swap32 v
    else v

  let get_int64 s off =
    let v = get_64 s off in
    if Sys.big_endian
    then swap64 v
    else v

  let set_int16 s off v =
    let v =
      if Sys.big_endian
      then swap16 v
      else v in
    set_16 s off v

  let set_int32 s off v =
    let v =
      if Sys.big_endian
      then swap32 v
      else v in
    set_32 s off v

  let set_int64 s off v =
    let v =
      if Sys.big_endian
      then swap64 v
      else v in
    set_64 s off v
end

module LE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

  let get_uint16 s off =
    let v = unsafe_get_16 s off in
    if Sys.big_endian
    then swap16 v
    else v

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let v = unsafe_get_32 s off in
    if Sys.big_endian
    then swap32 v
    else v

  let get_int64 s off =
    let v = unsafe_get_64 s off in
    if Sys.big_endian
    then swap64 v
    else v

  let set_int16 s off v =
    let v =
      if Sys.big_endian
      then swap16 v
      else v in
    set_16 s off v

  let set_int32 s off v =
    let v =
      if Sys.big_endian
      then swap32 v
      else v in
    set_32 s off v

  let set_int64 s off v =
    let v =
      if Sys.big_endian
      then swap64 v
      else v in
    set_64 s off v
end

#else

module BE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

  let get_uint16 s off =
    let hi = get_uint8 s off in
    let lo = get_uint8 s (off+1) in
    (hi lsl 8) + lo

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let hi = get_uint16 s off in
    let lo = get_uint16 s (off+2) in
    Int32.(logor (shift_left (of_int hi) 16) (of_int lo))

  let get_int64 s off =
    let hi = get_int32 s off in
    let lo = get_int32 s (off+4) in
    Int64.(logor (shift_left (of_int32 hi) 32) (logand (of_int32 lo) 0xffffffff_L))

  let set_int16 s off v =
    set_int8 s off (v lsr 8);
    set_int8 s (off+1) (v land 0xff)

  let set_int32 s off v =
    set_int16 s off (Int32.(to_int (shift_right_logical v 16)));
    set_int16 s (off+2) (Int32.(to_int (logand v 0xffff_l)))

  let set_int64 s off v =
    set_int32 s off (Int64.(to_int32 (shift_right_logical v 32)));
    set_int32 s (off+4) (Int64.(to_int32 (logand v 0xffffffff_L)))

end

module BE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

  let get_uint16 s off =
    let hi = get_uint8 s off in
    let lo = get_uint8 s (off+1) in
    (hi lsl 8) + lo

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let hi = get_uint16 s off in
    let lo = get_uint16 s (off+2) in
    Int32.(logor (shift_left (of_int hi) 16) (of_int lo))

  let get_int64 s off =
    let hi = get_int32 s off in
    let lo = get_int32 s (off+4) in
    Int64.(logor (shift_left (of_int32 hi) 32) (logand (of_int32 lo) 0xffffffff_L))

  let set_int16 s off v =
    set_int8 s off (v lsr 8);
    set_int8 s (off+1) (v land 0xff)

  let set_int32 s off v =
    set_int16 s off (Int32.(to_int (shift_right_logical v 16)));
    set_int16 s (off+2) (Int32.(to_int (logand v 0xffff_l)))

  let set_int64 s off v =
    set_int32 s off (Int64.(to_int32 (shift_right_logical v 32)));
    set_int32 s (off+4) (Int64.(to_int32 (logand v 0xffffffff_L)))

end

module LE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

  let get_uint16 s off =
    let hi = get_uint8 s (off+1) in
    let lo = get_uint8 s off in
    (hi lsl 8) + lo

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let hi = get_uint16 s (off+2) in
    let lo = get_uint16 s off in
    Int32.(logor (shift_left (of_int hi) 16) (of_int lo))

  let get_int64 s off =
    let hi = get_int32 s (off+4) in
    let lo = get_int32 s off in
    Int64.(logor (shift_left (of_int32 hi) 32) (logand (of_int32 lo) 0xffffffff_L))

  let set_int16 s off v =
    set_int8 s (off+1) (v lsr 8);
    set_int8 s off (v land 0xff)

  let set_int32 s off v =
    set_int16 s (off+2) (Int32.(to_int (shift_right_logical v 16)));
    set_int16 s off (Int32.(to_int (logand v 0xffff_l)))

  let set_int64 s off v =
    set_int32 s (off+4) (Int64.(to_int32 (shift_right_logical v 32)));
    set_int32 s off (Int64.(to_int32 (logand v 0xffffffff_L)))

end

module LE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

  let get_uint16 s off =
    let hi = get_uint8 s (off+1) in
    let lo = get_uint8 s off in
    (hi lsl 8) + lo

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    let hi = get_uint16 s (off+2) in
    let lo = get_uint16 s off in
    Int32.(logor (shift_left (of_int hi) 16) (of_int lo))

  let get_int64 s off =
    let hi = get_int32 s (off+4) in
    let lo = get_int32 s off in
    Int64.(logor (shift_left (of_int32 hi) 32) (logand (of_int32 lo) 0xffffffff_L))

  let set_int16 s off v =
    set_int8 s (off+1) (v lsr 8);
    set_int8 s off (v land 0xff)

  let set_int32 s off v =
    set_int16 s (off+2) (Int32.(to_int (shift_right_logical v 16)));
    set_int16 s off (Int32.(to_int (logand v 0xffff_l)))

  let set_int64 s off v =
    set_int32 s (off+4) (Int64.(to_int32 (shift_right_logical v 32)));
    set_int32 s off (Int64.(to_int32 (logand v 0xffffffff_L)))

end

#endif
