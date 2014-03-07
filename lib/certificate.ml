open Registry
open Asn_grammars
open Asn
open Utils

type certificate_failure =
  | InvalidCertificate
  | InvalidSignature
  | InvalidServerName
  | SelfSigned
  | MultipleRootCA
  | NoTrustAnchor
  | NoServerName

type verification_result = [
  | `Fail of certificate_failure
  | `Ok
]


(* 5280 A certificate MUST NOT include more than one instance of a particular extension. *)

let issuer_matches_subject_tbs : tBSCertificate -> tBSCertificate -> bool =
  fun p c -> Name.equal p.subject c.issuer

let issuer_matches_subject : certificate -> certificate -> bool =
  fun p c -> issuer_matches_subject_tbs p.tbs_cert c.tbs_cert

let is_self_signed : certificate -> bool =
  fun c -> issuer_matches_subject c c

let validate_signature : certificate -> certificate -> Cstruct.t -> bool =
  fun trusted c raw ->
   try (

     (* How could a silent exception be better than a compiler warning on
      * non-exhaustiveness!? *)
(*     let issuing_key = match trusted.tbs_cert.pk_info with
      | PK.RSA key -> key
      |  _         -> assert false
    in *)
    let (PK.RSA issuing_key) = trusted.tbs_cert.pk_info in

    (* issuer of c should be subject of trusted! *)
    assert (issuer_matches_subject trusted c);

    (* XXX: this is awful code! *)
    let siglen = Cstruct.len c.signature_val in
    (* not sure whether 128 is what we want here, for sure we just want to translate the certificate to a cstruct ;) *)
    let off = if siglen > 128 then 1 else 0 in
    (* 4 is the prefix-seq, 19 the sig oid *)
    let to_hash = Cstruct.sub raw 4 ((Cstruct.len raw) - (siglen + 4 + 19 + off)) in
    (* this results in a different encoding than the original certificate *)
    (* let dat = tbs_certificate_to_cstruct c.tbs_cert in
       assert (Utils.cs_eq to_hash dat); *) (* david: this fails *)
    let signature = Crypto.verifyRSA_and_unpadPKCS1 issuing_key c.signature_val in
    let algo, hash = match pkcs1_digest_info_of_cstruct signature with
      | Some ((a, b), _) -> (a, b)
      | None -> assert false
    in

    (* XXX move me outside of that comment up there? *)
    let comparing_hash hashfn =
      let chash = hashfn to_hash in
      Utils.cs_eq chash hash in

    let open Algorithm in
    match (c.signature_algo, algo) with
    | (MD5_RSA, MD5)   -> comparing_hash Crypto.md5
    | (SHA1_RSA, SHA1) -> comparing_hash Crypto.sha
    | _                -> false)
   with
   | _ -> false


let validate_time now cert =
  let from, till = cert.validity in
(* TODO:  from < now && now < till *)
  true

let validate_ca_extensions cert =
  try (
    let open Extension in
    (* comments from RFC5280 *)
    (* 4.2.1.9 Basic Constraints *)
    (* Conforming CAs MUST include this extension in all CA certificates used *)
    (* to validate digital signatures on certificates and MUST mark the *)
    (* extension as critical in such certificates *)
    let bc =
      function
        (* unfortunately, there are 12 CA certs (including the one which
           signed google.com) which are _NOT_ marked as critical *)
      | (_, Basic_constraints _) -> true
      | _                        -> false
    in
    assert (List.exists bc cert.extensions);

    (* 4.2.1.3 Key Usage *)
    (* Conforming CAs MUST include key usage extension *)
    (* CA Cert (cacert.org) does not *)
    let ku =
      function
      | (_, Key_usage k) ->
         (* When present, conforming CAs SHOULD mark this extension as critical *)
         (* yeah, you wish... *)
         List.exists (function
                       | Key_cert_sign -> true
                       | _             -> false)
                     k
      | _ -> false
    in
    assert (List.exists ku cert.extensions);

    (* Name Constraints   - name constraints should match servername *)

    let rec ver_ext =
      function
      | []                                 -> true
      | (true,  Key_usage _)         :: xs -> ver_ext xs
      | (true,  Basic_constraints _) :: xs -> ver_ext xs
      (* we've to deal with _all_ extensions marked critical! *)
      | (true,  _)                   :: xs -> false
      | (false, _)                   :: xs -> ver_ext xs
    in
    ver_ext cert.extensions) with
  | _ -> false


let ext_authority_matches_subject trusted cert =
  let open Extension in
  match List.filter (function
                      | (_, Authority_key_id (Some _, _, _)) -> true
                      | _ -> false)
                    cert.extensions with
  | [] -> true (* it's not mandatory *)
  | [(_, Authority_key_id (Some auth, _, _))] ->
     List.exists (function
                   | (_, Subject_key_id au) ->
                      let res = Utils.cs_eq auth au in
                      Printf.printf "SUB KEY ID and AUTH KEY ID matches? %s" (string_of_bool res);
                      Cstruct.hexdump auth;
                      Cstruct.hexdump au;
                      res
                   | _                 -> false)
                 trusted.extensions
  | _ -> false (* shouldn't happen *)

let validate_intermediate_extensions trusted cert =
  (validate_ca_extensions cert) && (ext_authority_matches_subject trusted cert)

let validate_server_extensions trusted cert =
  let open Extension in
  let rec ver_ext =
    function
    | []                                        -> true
    | (_,     Basic_constraints (Some x)) :: xs -> false
    | (_,     Key_usage us)               :: xs ->
       List.exists (function
                     (* key_encipherment (RSA) *)
                     (* signing (DHE_RSA) *)
                     | Key_encipherment -> true
                     | _                -> false)
                   us
    | (_,     Ext_key_usage us)           :: xs ->
       List.exists (function
                     | Server_auth      -> true
                     | _                -> false)
                   us
    | (false, _)                          :: xs -> ver_ext xs
    (* we've to deal with _all_ extensions marked critical! *)
    | (true,  _)                          :: xs -> false
  in
  (ver_ext cert.extensions) && (ext_authority_matches_subject trusted cert)

let get_cn cert =
  map_find cert.subject
    ~f:Name.(function Common_name n -> Some n | _ -> None)

let get_common_name cert =
  match get_cn cert.tbs_cert with
  | None   ->
     let sigl = Cstruct.len cert.signature_val in
     let sign = Cstruct.copy cert.signature_val 0 sigl in
     let hex = Cryptokit.(transform_string (Hexa.encode ()) sign) in
     "NO commonName " ^ hex
  | Some x -> x

let verify_certificate : certificate -> float -> string option -> certificate -> Cstruct.t -> verification_result =
  fun trusted now servername c raw ->
    Printf.printf "verify certificate %s -> %s\n"
                  (get_common_name trusted)
                  (get_common_name c);
    let cert = c.tbs_cert in
    match
      validate_signature trusted c raw &&
      validate_time now cert           &&
      validate_intermediate_extensions trusted.tbs_cert cert
    with
    | true -> `Ok
    | _    -> `Fail InvalidCertificate

let verify_ca_cert now cert raw =
  Printf.printf "verifying CA cert %s: " (get_common_name cert);
  let tbs = cert.tbs_cert in
  match
    validate_signature cert cert raw &&
    validate_time now tbs            &&
    validate_ca_extensions tbs
  with
  | true -> Printf.printf "ok\n";     true
  | _    -> Printf.printf "failed\n"; false

let find_trusted_certs : float -> certificate list =
  fun now ->
    let cacert, raw = Crypto_utils.cert_of_file "../certificates/cacert.crt" in
    let nss = Crypto_utils.certs_of_file "../certificates/ca-root-nss.crt" in
    let cas = List.append nss [(cacert, raw)] in
    let valid = List.filter (fun (cert, raw) -> verify_ca_cert now cert raw) cas in
    Printf.printf "read %d certificates, could validate %d\n" (List.length cas) (List.length valid);
    let certs, _ = List.split valid in
    certs

let hostname_matches : tBSCertificate -> string -> bool =
  fun cert name ->
  try (
    let open Extension in
    (* - might include wildcards and international domain names *)
    let rec ver_sn = function
      | []                                -> false
      | (_, Subject_alt_name names) :: xs ->
         let open General_name in
         assert (List.exists (function
                               | DNS x -> x = name
                               | _     -> false)
                             names);
         true
      | _                           :: xs -> ver_sn xs
    in
    let cn_eq = match get_cn cert with
      | None   -> false
      | Some x -> x = name
    in
    (ver_sn cert.extensions) || cn_eq
  ) with
  | _ -> false

let verify_server_certificate : certificate -> float -> string option -> certificate -> Cstruct.t -> verification_result =
  fun trusted now servername c raw ->
  Printf.printf "verify server certificate %s -> %s\n"
                (get_common_name trusted)
                (get_common_name c);
  let cert = c.tbs_cert in
  let smatches = fun name c ->
    match name with
    | None   -> false
    | Some x -> hostname_matches c x
  in
  match
    validate_signature trusted c raw                 &&
    validate_time now cert                           &&
    validate_server_extensions trusted.tbs_cert cert &&
    smatches servername cert
  with
  | true ->
      Printf.printf "successfully verified server certificate\n";
      `Ok
  | _ ->
      Printf.printf "could not verify server certificate\n";
      `Fail InvalidCertificate

let verify_top_certificate : certificate list -> float -> string option -> certificate -> Cstruct.t -> verification_result =
  fun trusted now servername c raw ->
    (* first have to find issuer of ``c`` in ``trusted`` *)
    Printf.printf "verify top certificate %s (%d CAs)\n"
                  (get_common_name c)
                  (List.length trusted);
    match List.filter (fun p -> issuer_matches_subject p c) trusted with
     | []  -> Printf.printf "couldn't find trusted CA cert\n"; `Fail NoTrustAnchor
     | [t] -> verify_certificate t now servername c raw
     | _   -> Printf.printf "found multiple root CAs\n"; `Fail MultipleRootCA

(* this is the API for a user (Cstruct.t list might go away) *)
let verify_certificates : string option -> certificate list -> Cstruct.t list -> verification_result =
  fun servername cs packets ->
    (* we get the certificate chain cs:
        [c0; c1; c2; ... ; cn]
        let server = c0
        let top = cn
       strategy:
        1. find a trusted CA for top and
             verify that trusted signed top
        2. verify intermediate certificates:
             verify that [cn .. c2] signed [cn-1 .. c1]
        3. verify server certificate was signed by c1 and
             server certificate has required servername *)
    (* short-path for self-signed certificate  *)
    if (List.length cs = 1) && is_self_signed (List.hd cs) then
      (* further verification of a self-signed certificate does not make sense:
         why should anyone handle a properly self-signed and valid certificate
         different from a badly signed invalid certificate? *)
      (Printf.printf "DANGER: self-signed certificate\n";
       `Fail SelfSigned)
    else
      let now = Sys.time () in
      let rec go t = function
        | []    -> (`Ok, t)
        | (c, p)::cs -> (* step 2 *)
           match verify_certificate t now servername c p with
           | `Ok  -> go c cs
           | `Fail x -> (`Fail x, c)
      in
      let trusted = find_trusted_certs now in
      (* server certificate *)
      let server, serverraw = (List.hd cs, List.hd packets) in
      (* intermediate certificates *)
      let reversed = List.combine (List.rev (List.tl cs)) (List.rev (List.tl packets)) in
      let topc, topr = List.hd reversed in
      (* step 1 *)
      match verify_top_certificate trusted now servername topc topr with
      | `Ok ->
         (* call step 2 *)
         (match go topc (List.tl reversed) with
          | (`Ok, t) ->
             (* step 3 *)
             verify_server_certificate t now servername server serverraw
          | (`Fail x, _) -> `Fail x)
      | `Fail x -> `Fail x


(* TODO: how to deal with
    2.16.840.1.113730.1.1 - Netscape certificate type
    2.16.840.1.113730.1.12 - SSL server name
    2.16.840.1.113730.1.13 - Netscape certificate comment *)

(* stuff from 4366 (TLS extensions):
  - root CAs
  - client cert url *)

(* Future TODO Certificate Revocation Lists and OCSP (RFC6520)
2.16.840.1.113730.1.2 - Base URL
2.16.840.1.113730.1.3 - Revocation URL
2.16.840.1.113730.1.4 - CA Revocation URL
2.16.840.1.113730.1.7 - Renewal URL
2.16.840.1.113730.1.8 - Netscape CA policy URL

2.5.4.38 - id-at-authorityRevocationList
2.5.4.39 - id-at-certificateRevocationList

2.5.29.20 - CRL Number
2.5.29.21 - reason code
2.5.29.27 - Delta CRL indicator
2.5.29.28 - Issuing Distribution Point
2.5.29.31 - CRL Distribution Points
2.5.29.46 - FreshestCRL

do not forget about 'authority information access' (private internet extension -- 4.2.2 of 5280) *)

(* Future TODO: Policies
2.5.29.32 - Certificate Policies
2.5.29.33 - Policy Mappings
2.5.29.36 - Policy Constraints
 *)

(* Future TODO: anything with subject_id and issuer_id ? seems to be not used by anybody *)

(* - test setup (ACM CCS'12):
            self-signed cert with requested commonName,
            self-signed cert with other commonName,
            valid signed cert with other commonName
   - also of interest: international domain names, wildcards *)

(* alternative approach: interface and implementation for certificate pinning *)
(* alternative approach': notary system / perspectives *)
(* alternative approach'': static list of trusted certificates *)
