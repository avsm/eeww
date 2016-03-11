let pbkdf1 hash password salt count dk_len hlen =
  let rec loop t = function
      0 -> t
    | i -> loop (hash t) (i - 1)
  in
  if dk_len > hlen 
  then invalid_arg "derived key too long"
  else Cstruct.sub (loop (Cstruct.concat [password; salt]) count) 0 dk_len
