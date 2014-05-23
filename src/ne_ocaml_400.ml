  let get_uint16 s off =
    if Sys.big_endian
    then
      let hi = get_uint8 s off in
      let lo = get_uint8 s (off+1) in
      (hi lsl 8) + lo
    else
      let hi = get_uint8 s (off+1) in
      let lo = get_uint8 s off in
      (hi lsl 8) + lo

  let get_int16 s off =
    sign16 (get_uint16 s off)

  let get_int32 s off =
    if Sys.big_endian
    then
      let hi = get_uint16 s off in
      let lo = get_uint16 s (off+2) in
      Int32.(logor (shift_left (of_int hi) 16) (of_int lo))
    else
      let hi = get_uint16 s (off+2) in
      let lo = get_uint16 s off in
      Int32.(logor (shift_left (of_int hi) 16) (of_int lo))

  let get_int64 s off =
    if Sys.big_endian
    then
      Int64.(logor (shift_left (of_int32 (get_int32 s off)) 32)
               (logand (of_int32 (get_int32 s (off+4))) 0xffffffff_L))
    else
      Int64.(logor (shift_left (of_int32 (get_int32 s (off+4))) 32)
               (logand (of_int32 (get_int32 s off)) 0xffffffff_L))

  let set_int16 s off v =
    if Sys.big_endian
    then begin
      set_int8 s off (v lsr 8);
      set_int8 s (off+1) (v land 0xff)
    end
    else begin
      set_int8 s (off+1) (v lsr 8);
      set_int8 s off (v land 0xff)
    end

  let set_int32 s off v =
    if Sys.big_endian
    then begin
      set_int16 s off (Int32.(to_int (shift_right_logical v 16)));
      set_int16 s (off+2) (Int32.(to_int (logand v 0xffff_l)))
    end
    else begin
      set_int16 s (off+2) (Int32.(to_int (shift_right_logical v 16)));
      set_int16 s off (Int32.(to_int (logand v 0xffff_l)))
    end

  let set_int64 s off v =
    if Sys.big_endian
    then begin
      set_int32 s off (Int64.(to_int32 (shift_right_logical v 32)));
      set_int32 s (off+4) (Int64.(to_int32 (logand v 0xffffffff_L)))
    end
    else begin
      set_int32 s (off+4) (Int64.(to_int32 (shift_right_logical v 32)));
      set_int32 s off (Int64.(to_int32 (logand v 0xffffffff_L)))
    end
