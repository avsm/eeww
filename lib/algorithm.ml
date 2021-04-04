open Asn.S
open Asn_grammars

(* This type really conflates three things: the set of pk algos that describe
 * the public key, the set of hashes, and the set of hash+pk algo combinations
 * that describe digests. The three are conflated because they are generated by
 * the same ASN grammar, AlgorithmIdentifier, to keep things close to the
 * standards.
 *
 * It's expected that downstream code with pick a subset and add a catch-all
 * that handles unsupported algos anyway.
 *)

type signature  = [ `RSA | `ECDSA | `ED25519 ]

type ec_curve =
  [ `SECP224R1 | `SECP256R1 | `SECP384R1 | `SECP521R1 | `Other of Asn.oid ]

let ec_curve_to_string = function
  | `SECP224R1 -> "SECP224R1"
  | `SECP256R1 -> "SECP256R1"
  | `SECP384R1 -> "SECP384R1"
  | `SECP521R1 -> "SECP521R1"
  | `Other oid -> "EC OID " ^ Fmt.to_to_string Asn.OID.pp oid

type t =

  (* pk algos *)
  (* any more? is the universe big enough? ramsey's theorem for pk cyphers? *)
  | RSA
  | EC_pub of ec_curve

  (* sig algos *)
  | MD2_RSA
  | MD4_RSA
  | MD5_RSA
  | RIPEMD160_RSA
  | SHA1_RSA
  | SHA256_RSA
  | SHA384_RSA
  | SHA512_RSA
  | SHA224_RSA
  | ECDSA_SHA1
  | ECDSA_SHA224
  | ECDSA_SHA256
  | ECDSA_SHA384
  | ECDSA_SHA512

  | ED25519

  (* digest algorithms *)
  | MD2
  | MD4
  | MD5
  | SHA1
  | SHA256
  | SHA384
  | SHA512
  | SHA224
  | SHA512_224
  | SHA512_256

let to_string = function
  | RSA -> "RSA"
  | EC_pub curve -> ec_curve_to_string curve
  | MD2_RSA -> "RSA MD2"
  | MD4_RSA -> "RSA MD4"
  | MD5_RSA -> "RSA MD5"
  | RIPEMD160_RSA -> "RSA RIPEMD160"
  | SHA1_RSA -> "RSA SHA1"
  | SHA256_RSA -> "RSA SHA256"
  | SHA384_RSA -> "RSA SHA384"
  | SHA512_RSA -> "RSA SHA512"
  | SHA224_RSA -> "RSA SHA224"
  | ECDSA_SHA1 -> "ECDSA SHA1"
  | ECDSA_SHA224 -> "ECDSA SHA224"
  | ECDSA_SHA256 -> "ECDSA SHA256"
  | ECDSA_SHA384 -> "ECDSA SHA384"
  | ECDSA_SHA512 -> "ECDSA SHA512"
  | ED25519 -> "Ed25519"
  | MD2 -> "MD2"
  | MD4 -> "MD4"
  | MD5 -> "MD5"
  | SHA1 -> "SHA1"
  | SHA256 -> "SHA256"
  | SHA384 -> "SHA384"
  | SHA512 -> "SHA512"
  | SHA224 -> "SHA224"
  | SHA512_224 -> "SHA512/224"
  | SHA512_256 -> "SHA512/256"

let to_hash = function
  | MD5    -> Some `MD5
  | SHA1   -> Some `SHA1
  | SHA224 -> Some `SHA224
  | SHA256 -> Some `SHA256
  | SHA384 -> Some `SHA384
  | SHA512 -> Some `SHA512
  | _      -> None

and of_hash = function
  | `MD5    -> MD5
  | `SHA1   -> SHA1
  | `SHA224 -> SHA224
  | `SHA256 -> SHA256
  | `SHA384 -> SHA384
  | `SHA512 -> SHA512

and to_key_type = function
  | RSA        -> Some `RSA
  | EC_pub curve -> Some (`EC curve)
  | ED25519    -> Some `ED25519
  | _          -> None

and of_key_type = function
  | `RSA    -> RSA
  | `EC curve -> EC_pub curve
  | `ED25519 -> ED25519

(* XXX: No MD2 / MD4 / RIPEMD160 *)
and to_signature_algorithm = function
  | MD5_RSA       -> Some (`RSA  , `MD5)
  | SHA1_RSA      -> Some (`RSA  , `SHA1)
  | SHA256_RSA    -> Some (`RSA  , `SHA256)
  | SHA384_RSA    -> Some (`RSA  , `SHA384)
  | SHA512_RSA    -> Some (`RSA  , `SHA512)
  | SHA224_RSA    -> Some (`RSA  , `SHA224)
  | ECDSA_SHA1    -> Some (`ECDSA, `SHA1)
  | ECDSA_SHA224  -> Some (`ECDSA, `SHA224)
  | ECDSA_SHA256  -> Some (`ECDSA, `SHA256)
  | ECDSA_SHA384  -> Some (`ECDSA, `SHA384)
  | ECDSA_SHA512  -> Some (`ECDSA, `SHA512)
  | ED25519       -> Some (`ED25519, `SHA512)
  | _             -> None

and[@ocaml.warning "-8"] of_signature_algorithm public_key_algorithm digest =
  match public_key_algorithm, digest with
  | (`RSA  , `MD5)    -> MD5_RSA
  | (`RSA  , `SHA1)   -> SHA1_RSA
  | (`RSA  , `SHA256) -> SHA256_RSA
  | (`RSA  , `SHA384) -> SHA384_RSA
  | (`RSA  , `SHA512) -> SHA512_RSA
  | (`RSA  , `SHA224) -> SHA224_RSA
  | (`ECDSA, `SHA1)   -> ECDSA_SHA1
  | (`ECDSA, `SHA224) -> ECDSA_SHA224
  | (`ECDSA, `SHA256) -> ECDSA_SHA256
  | (`ECDSA, `SHA384) -> ECDSA_SHA384
  | (`ECDSA, `SHA512) -> ECDSA_SHA512
  | (`ED25519, _) -> ED25519

(* XXX
 *
 * PKCS1/RFC5280 allows params to be `ANY', depending on the algorithm.  I don't
 * know of one that uses anything other than NULL and OID, however, so we accept
 * only that.

   RFC 3279 Section 2.2.1 defines for RSA Signature Algorithms SHALL have null
   as parameter, but certificates in the wild don't contain the parameter field
   at all (it is optional). We accept both, and output a null paramter.
   Section 2.2.2 specifies DSA to have a null parameter,
   Section 2.2.3 specifies ECDSA to have a null parameter,
   Section 2.3.1 specifies rsaEncryption (for RSA public keys) requires null.
 *)

let identifier =
  let open Registry in

  let f =
    let none x = function
      | None -> x
      | _    -> parse_error "Algorithm: expected no parameters"
    and null x = function
      | Some (`C1 ()) -> x
      | _             -> parse_error "Algorithm: expected null parameters"
    and null_or_none x = function
      | None | Some (`C1 ()) -> x
      | _                    -> parse_error "Algorithm: expected null or none parameter"
    and oid f = function
      | Some (`C2 id) -> f id
      | _             -> parse_error "Algorithm: expected parameter OID"
    and default oid = Asn.(S.parse_error "Unknown algorithm %a" OID.pp oid)
    and curve =
      let default oid = `Other oid in
      case_of_oid ~default [
        (ANSI_X9_62.secp224r1, `SECP224R1) ;
        (ANSI_X9_62.secp256r1, `SECP256R1) ;
        (ANSI_X9_62.secp384r1, `SECP384R1) ;
        (ANSI_X9_62.secp521r1, `SECP521R1) ;
      ]
    in

    case_of_oid_f ~default [

      (ANSI_X9_62.ec_pub_key, oid (fun id -> EC_pub (curve id))) ;

      (PKCS1.rsa_encryption          , null RSA                  ) ;
      (PKCS1.md2_rsa_encryption      , null_or_none MD2_RSA      ) ;
      (PKCS1.md4_rsa_encryption      , null_or_none MD4_RSA      ) ;
      (PKCS1.md5_rsa_encryption      , null_or_none MD5_RSA      ) ;
      (PKCS1.ripemd160_rsa_encryption, null_or_none RIPEMD160_RSA) ;
      (PKCS1.sha1_rsa_encryption     , null_or_none SHA1_RSA     ) ;
      (PKCS1.sha256_rsa_encryption   , null_or_none SHA256_RSA   ) ;
      (PKCS1.sha384_rsa_encryption   , null_or_none SHA384_RSA   ) ;
      (PKCS1.sha512_rsa_encryption   , null_or_none SHA512_RSA   ) ;
      (PKCS1.sha224_rsa_encryption   , null_or_none SHA224_RSA   ) ;

      (ANSI_X9_62.ecdsa_sha1         , none ECDSA_SHA1   ) ;
      (ANSI_X9_62.ecdsa_sha224       , none ECDSA_SHA224 ) ;
      (ANSI_X9_62.ecdsa_sha256       , none ECDSA_SHA256 ) ;
      (ANSI_X9_62.ecdsa_sha384       , none ECDSA_SHA384 ) ;
      (ANSI_X9_62.ecdsa_sha512       , none ECDSA_SHA512 ) ;

      (RFC8410.ed25519               , none ED25519 ) ;

      (md2                           , null MD2          ) ;
      (md4                           , null MD4          ) ;
      (md5                           , null MD5          ) ;
      (sha1                          , null SHA1         ) ;
      (sha256                        , null SHA256       ) ;
      (sha384                        , null SHA384       ) ;
      (sha512                        , null SHA512       ) ;
      (sha224                        , null SHA224       ) ;
      (sha512_224                    , null SHA512_224   ) ;
      (sha512_256                    , null SHA512_256   ) ]

  and g =
    let none    = None
    and null    = Some (`C1 ())
    and oid  id = Some (`C2 id)
    and curve = function
      | `SECP224R1 -> ANSI_X9_62.secp224r1
      | `SECP256R1 -> ANSI_X9_62.secp256r1
      | `SECP384R1 -> ANSI_X9_62.secp384r1
      | `SECP521R1 -> ANSI_X9_62.secp521r1
      | `Other oid -> oid
    in
    function
    | EC_pub id     -> (ANSI_X9_62.ec_pub_key , oid (curve id))

    | RSA           -> (PKCS1.rsa_encryption           , null)
    | MD2_RSA       -> (PKCS1.md2_rsa_encryption       , null)
    | MD4_RSA       -> (PKCS1.md4_rsa_encryption       , null)
    | MD5_RSA       -> (PKCS1.md5_rsa_encryption       , null)
    | RIPEMD160_RSA -> (PKCS1.ripemd160_rsa_encryption , null)
    | SHA1_RSA      -> (PKCS1.sha1_rsa_encryption      , null)
    | SHA256_RSA    -> (PKCS1.sha256_rsa_encryption    , null)
    | SHA384_RSA    -> (PKCS1.sha384_rsa_encryption    , null)
    | SHA512_RSA    -> (PKCS1.sha512_rsa_encryption    , null)
    | SHA224_RSA    -> (PKCS1.sha224_rsa_encryption    , null)

    | ECDSA_SHA1    -> (ANSI_X9_62.ecdsa_sha1          , none)
    | ECDSA_SHA224  -> (ANSI_X9_62.ecdsa_sha224        , none)
    | ECDSA_SHA256  -> (ANSI_X9_62.ecdsa_sha256        , none)
    | ECDSA_SHA384  -> (ANSI_X9_62.ecdsa_sha384        , none)
    | ECDSA_SHA512  -> (ANSI_X9_62.ecdsa_sha512        , none)

    | ED25519       -> (RFC8410.ed25519                , none)

    | MD2           -> (md2                            , null)
    | MD4           -> (md4                            , null)
    | MD5           -> (md5                            , null)
    | SHA1          -> (sha1                           , null)
    | SHA256        -> (sha256                         , null)
    | SHA384        -> (sha384                         , null)
    | SHA512        -> (sha512                         , null)
    | SHA224        -> (sha224                         , null)
    | SHA512_224    -> (sha512_224                     , null)
    | SHA512_256    -> (sha512_256                     , null)
  in

  map f g @@
  sequence2
    (required ~label:"algorithm" oid)
    (optional ~label:"params" (choice2 null oid))

let ecdsa_sig =
  let f (r, s) =
    if Z.sign r < 0 then
      Asn.S.parse_error "ECDSA signature: r < 0"
    else if Z.sign s < 0 then
      Asn.S.parse_error "ECDSA signature: s < 0"
    else
      Mirage_crypto_pk.Z_extra.to_cstruct_be r,
      Mirage_crypto_pk.Z_extra.to_cstruct_be s
  and g (r, s) =
    Mirage_crypto_pk.Z_extra.of_cstruct_be r,
    Mirage_crypto_pk.Z_extra.of_cstruct_be s
  in
  map f g @@
  sequence2
    (required ~label:"r" integer)
    (required ~label:"s" integer)

let ecdsa_sig_of_cstruct, ecdsa_sig_to_cstruct =
  projections_of Asn.der ecdsa_sig
