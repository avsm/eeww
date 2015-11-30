
module type S = sig
    val extract : ?salt:Cstruct.t -> Cstruct.t -> Cstruct.t
    val expand : prk:Cstruct.t -> ?info:Cstruct.t -> int -> Cstruct.t
end

module Make (H : Nocrypto.Hash.S) = struct

  open Nocrypto.Hash

  let extract ?salt ikm =
    let key = match salt with
      | None -> let buf = Cstruct.create H.digest_size in
                Cstruct.memset buf 0 ;
                buf
      | Some x -> x
    in
    H.hmac ~key ikm

  let expand ~prk ?info len =
    let info = match info with
      | None -> Cstruct.create 0
      | Some x -> x
    in
    let t n last =
      let nc = Cstruct.create 1 in
      Cstruct.set_uint8 nc 0 n ;
      H.hmac ~key:prk (Cstruct.concat [last ; info ; nc])
    in
    let n = succ (len / H.digest_size) in
    let rec compute acc count = match count, acc with
      | c, xs when c > n -> Cstruct.concat (List.rev xs)
      | c, x::_ -> compute (t c x :: acc) (succ c)
      | _, [] -> invalid_arg "can not happen"
    in
    let buf = compute [Cstruct.create 0] 1 in
    Cstruct.sub buf 0 len
end

module MD5HKDF = Make(Nocrypto.Hash.MD5)
module SHA1HKDF = Make(Nocrypto.Hash.SHA1)
module SHA224HKDF = Make(Nocrypto.Hash.SHA224)
module SHA256HKDF = Make(Nocrypto.Hash.SHA256)
module SHA384HKDF = Make(Nocrypto.Hash.SHA384)
module SHA512HKDF = Make(Nocrypto.Hash.SHA512)

let extract ~hash ?salt ikm =
  match hash with
  | `MD5 -> MD5HKDF.extract ?salt ikm
  | `SHA1 -> SHA1HKDF.extract ?salt ikm
  | `SHA224 -> SHA224HKDF.extract ?salt ikm
  | `SHA256 -> SHA256HKDF.extract ?salt ikm
  | `SHA384 -> SHA384HKDF.extract ?salt ikm
  | `SHA512 -> SHA512HKDF.extract ?salt ikm

let expand ~hash ~prk ?info len =
  match hash with
  | `MD5 -> MD5HKDF.expand ~prk ?info len
  | `SHA1 -> SHA1HKDF.expand ~prk ?info len
  | `SHA224 -> SHA224HKDF.expand ~prk ?info len
  | `SHA256 -> SHA256HKDF.expand ~prk ?info len
  | `SHA384 -> SHA384HKDF.expand ~prk ?info len
  | `SHA512 -> SHA512HKDF.expand ~prk ?info len
