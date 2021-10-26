  let get_uint16 s off =
    get_16 s off

  let get_int16 s off =
   ((get_uint16 s off) lsl ( Sys.int_size - 16 )) asr ( Sys.int_size - 16 )

  let get_int32 s off =
    get_32 s off

  let get_int64 s off =
    get_64 s off

  let set_int16 s off v =
    set_16 s off v

  let set_int32 s off v =
    set_32 s off v

  let set_int64 s off v =
    set_64 s off v
