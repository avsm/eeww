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
