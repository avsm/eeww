open Cohttp_eio
open Eio

module Conn = struct
  module S = struct
    type t = {
      label: string;
      net: Eio.Net.t;
    }
    let v ~label ~net = { label; net }
    let _pp fmt t = Fmt.str fmt "%s" t.label
  end

  let make ~s ~sw stream =
    Net.connect ~sw s.S.net stream
end

module Dns = struct

end

module Url = struct

  module Https = struct
    module S = struct
      type t = {
        label: string;
        net: Eio.Net.t;
        tls_config: Tls.Config.client;
        conns: Conn.S.t;
      }
      let v ~label ~net ~conns =
        let tls_config =
          let null ?ip:_ ~host:_ _certs = Ok None in
          Tls.Config.client ~authenticator:null () in
        { label; tls_config; net; conns }
    let _pp fmt t = Fmt.str fmt "%s" t.label
    end

    let fetch ~s (base, resource) =
      let headers = None in
      match Eio.Net.getaddrinfo_stream ~service:"https" s.S.net base with
      | [] -> failwith "host resolution failed"
      | stream :: _ ->
          Switch.run @@ fun sw -> (* XXX move *)
          let conn = Conn.make ~s:s.S.conns ~sw stream in
          let host = Domain_name.(of_string_exn base |> host |> Result.to_option) in
          let conn = Tls_eio.client_of_flow ?host s.S.tls_config conn in
          let resp = Client.get ?headers ~conn (base, None) resource in
          Client.read_fixed resp
  end

  module S = struct
    type t = {
      label: string;
      https: Https.S.t;
      (* TODO protocols *)
    }
    let v ~label ~https = { label; https } (* TODO dynamic protocal reg *)
    let _pp fmt t = Fmt.str fmt "%s" t.label
  end
  
  let fetch ~s uri =
    match Uri.scheme uri with
    | Some "https" -> Https.fetch ~s:s.S.https ((Option.get (Uri.host uri)), (Uri.path uri))
    | _ -> failwith "unknown schema"
end

let _ =

  (* the scheduler setup *)
  Eio_main.run @@ fun env ->
  Eio.Ctf.with_tracing @@ fun () ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Conn.S.v ~label:"conns" ~net:env#net |> fun conns ->
  Url.Https.S.v ~label:"https" ~net:env#net ~conns |> fun https ->
  Url.S.v ~label:"url" ~https |> fun url_s ->

  (* start of app code *)
  let url = Uri.of_string "https://anil.recoil.org/" in
  let body = Url.fetch ~s:url_s url  in
  Eio.traceln "body: %s" body
