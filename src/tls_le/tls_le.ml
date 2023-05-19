(* A direct-style interface to LetsEncrypt *)
module Token_cache = struct
  let h = Hashtbl.create 7

  let add k v =
    Hashtbl.replace h k v
    (* TODO mutex *)

  let get k =
    Hashtbl.find h k
    (* TODO mutex *)
end

exception Le_error of string
let errcheck = function Ok v -> v | Error (`Msg m) -> raise (Le_error m)

let (/) = Eio.Path.(/)

let gen_account_key ~account_file () =
  let privkey = `RSA (Mirage_crypto_pk.Rsa.generate ~bits:2048 ()) in
  let key_pem = X509.Private_key.encode_pem privkey |> Cstruct.to_string in
  Eio.Path.save ~create:(`Or_truncate 0o600) account_file key_pem

let gen_csr ~org ~email ~domain ~csr_file ~key_file () =
  let dn = X509.Distinguished_name.[
     Relative_distinguished_name.(singleton (CN domain));
     Relative_distinguished_name.(singleton (Mail email));
     Relative_distinguished_name.(singleton (O org));
  ] in
  let privkey = `RSA (Mirage_crypto_pk.Rsa.generate ~bits:4096 ()) in
  let csr = X509.Signing_request.create dn privkey |> errcheck in
  let csr_pem = X509.Signing_request.encode_pem csr |> Cstruct.to_string in
  let key_pem = X509.Private_key.encode_pem privkey |> Cstruct.to_string in
  Eio.Path.save ~create:(`Or_truncate 0o600) csr_file csr_pem;
  Eio.Path.save ~create:(`Or_truncate 0o600) key_file key_pem

let gen_cert ~csr_pem ~account_pem ~email ~cert_file ~endpoint env =
  let account_key = X509.Private_key.decode_pem (Cstruct.of_string account_pem) |> errcheck in
  let request = X509.Signing_request.decode_pem (Cstruct.of_string csr_pem) |> errcheck in
  let solver =
    let solve_challenge _ ~prefix:_ ~token ~content =
        Token_cache.add token content; Ok () in
        Letsencrypt.Client.http_solver solve_challenge in
  let sleep n = Eio.Time.sleep env#clock (float_of_int n) in
  let le = Letsencrypt.Client.initialise env ~endpoint ~email account_key |> errcheck in
  let certs = Letsencrypt.Client.sign_certificate env solver le sleep request |> errcheck in
  let cert = Cstruct.to_string @@ X509.Certificate.encode_pem_multiple certs in
  Eio.Path.save ~create:(`Or_truncate 0o600) cert_file cert

let get_tls_server_config ?alpn_protocols ~key_file ~cert_file () =
  let certificate = X509_eio.private_of_pems ~cert:cert_file ~priv_key:key_file in
  let certificates = `Single  certificate in
  Tls.Config.(server ?alpn_protocols ~version:(`TLS_1_0, `TLS_1_3) ~certificates ~ciphers:Ciphers.supported ())

module Eiox = struct
  (* UPSTREAM: need an Eio file exists check without opening *)
  let file_exists f =
    Eio.Switch.run @@ fun sw ->
    try ignore(Eio.Path.open_in ~sw f); true
    with _ -> false
end

let tls_config ?alpn_protocols ~cert_root ~org ~email ~domain ~endpoint env =
  let account_file = cert_root / "account.pem" in
  let csr_file = cert_root / "csr.pem" in
  let key_file = cert_root / "privkey.pem" in
  let cert_file = cert_root / "fullcert.pem" in
  if not (Eiox.file_exists account_file) then begin
    Eio.traceln "Generating account key";
    gen_account_key ~account_file ()
  end;
  if not (Eiox.file_exists key_file) then begin
    Eio.traceln "Generating key file and CSR";
    gen_csr ~org ~email ~domain ~csr_file ~key_file ();
  end;
  if not (Eiox.file_exists cert_file) then begin
    Eio.traceln "Generating cert file";
    let csr_pem = Eio.Path.load csr_file in
    let account_pem = Eio.Path.load account_file in
    gen_cert ~csr_pem ~account_pem ~email ~cert_file ~endpoint env
  end;
  get_tls_server_config ?alpn_protocols ~key_file ~cert_file ()
