open Cohttp_eio
open Eio

(** [Conn] manages TCP/Unix domain connections *)
module Conn = struct

  (** Conn scheduler *)
  module S = struct
    type t = {
      label: string;
      net: Eio.Net.t;
    }

    let v ~label ~net =
      traceln "Conn.S: init %s" label;
      { label; net }

    let _pp fmt t = Fmt.str fmt "%s" t.label
  end

  let make s ~sw stream =
    traceln "Conn.make: %s -> %a" s.S.label Net.Sockaddr.pp stream;
    (* TODO connection pooling here *)
    Net.connect ~sw s.S.net stream
end

(** [Url] fetches Uri bodies *)
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

    let fetch s (base, resource) =
      let headers = None in
      match Eio.Net.getaddrinfo_stream ~service:"https" s.S.net base with
      | [] -> failwith "host resolution failed"
      | stream :: _ ->
          Switch.run @@ fun sw -> (* XXX move *)
          let conn = Conn.make s.S.conns ~sw stream in
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
    let v ~label ~net =
      Conn.S.v ~label:"conns" ~net |> fun conns ->
      Https.S.v ~label:"https" ~net ~conns |> fun https ->
      {label; https}

    let _pp fmt t = Fmt.str fmt "%s" t.label
  end
  
  let fetch s uri =
    match Uri.scheme uri with
    | Some "https" ->
        Https.fetch s.S.https ((Option.get (Uri.host uri)), (Uri.path uri))
    | _ -> failwith "unknown schema"
end

let _ =
  (* the scheduler setup *)
  (* Bars.run (); *)

  Eio_main.run @@ fun env ->
  Eio.Ctf.with_tracing @@ fun () ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  (* TODO turn this into handler *)
  Url.S.v ~label:"url" ~net:env#net |> fun url_s ->

  (* start of app code *)
  let url = Uri.of_string "https://anil.recoil.org/" in
  let body = Url.fetch url_s url  in
  Eio.traceln "body: %d" (String.length body)
