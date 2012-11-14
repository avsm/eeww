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
