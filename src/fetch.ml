open Cohttp_eio
open Eio


let get_html ?headers ~tls_config ~net (base, resource) =
  match Eio.Net.getaddrinfo_stream ~service:"https" net base with
  | [] -> failwith "Host resolution failed"
  | stream :: _ ->
      Switch.run @@ fun sw ->
      let conn = Net.connect ~sw net stream in
      let conn =
        Tls_eio.client_of_flow tls_config
          ?host:
            (Domain_name.of_string_exn base
            |> Domain_name.host |> Result.to_option)
          conn
      in
      let resp = Client.get ?headers ~conn (base, None) resource in
      Client.read_fixed resp

let _ =
  Eio_main.run @@ fun env ->
  Eio.Ctf.with_tracing @@ fun () ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  let tls_config =
    let null ?ip:_ ~host:_ _certs = Ok None in
    Tls.Config.client ~authenticator:null () in
  let url = Uri.of_string "https://anil.recoil.org/" in
  let p = (Option.get (Uri.host url)), (Uri.path url) in
  let body = get_html ~tls_config ~net:env#net p in
  Eio.traceln "body: %s" body
