module type S = sig
  val pbkdf1 : password:Cstruct.t -> salt:Cstruct.t -> count:int -> dk_len:int -> Cstruct.t
end

module Make (H: Nocrypto.Hash.S) : S = struct
  let pbkdf1 ~password ~salt ~count ~dk_len =
    if Cstruct.len salt <> 8 then invalid_arg "salt should be 8 bytes"
    else if count < 0 then invalid_arg "count cannot be negative"
    else if dk_len < 0 then invalid_arg "derived key length cannot be negative"
    else if dk_len > H.digest_size then invalid_arg "derived key too long"
    else
      let rec loop t = function
          0 -> t
        | i -> loop (H.digest t) (i - 1)
      in
      Cstruct.sub (loop (Cstruct.append password salt) count) 0 dk_len
end

let pbkdf1 ~hash ~password ~salt ~count ~dk_len =
  let module H = (val (Nocrypto.Hash.module_of hash)) in
  let module PBKDF = Make (H) in
  PBKDF.pbkdf1 ~password ~salt ~count ~dk_len
