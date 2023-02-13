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
      mgr : Eio.Domain_manager.t;
      (* TODO protocols *)
    }

    type _ Effect.t += Dummy : unit Effect.t

    (* A dummy handler. And HTTP implementation will want to schedule based
       on connections perhaps or something like that. Here we just need to
       perform an effect to make sure we are doing things correctly. *)
    let handler =
      let open Effect.Deep in
      {
        retc = (fun v -> v);
        exnc = raise;
        effc = (fun (type a) (e : a Effect.t) -> match e with
          | Dummy -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None
        );
      }

    let http = Eio.Domain_manager.register_handler handler

    let v ~label ~mgr ~net =
      Conn.S.v ~label:"conns" ~net |> fun conns ->
      Https.S.v ~label:"https" ~net ~conns |> fun https ->
      { label; https; mgr }

    let _pp fmt t = Fmt.str fmt "%s" t.label
  end
  
  let fetch s uri =
    match Uri.scheme uri with
    | Some "https" ->
        let task () =
          Eio.traceln "starting HTTP fetch";
          (* Perform the dummy effect to ensure the appropriate handler
             has been installed into the domain where we run. *)
          Effect.perform S.Dummy;
          (* A bit of computational work to see the parallelism, otherwise
             our program is just IO bound. *)
          Unix.sleep 1;
          Https.fetch s.S.https ((Option.get (Uri.host uri)), (Uri.path uri))
        in
        Eio.Domain_manager.submit s.S.mgr S.http task
    | _ -> failwith "unknown schema"
end

let _ =
  (* the scheduler setup *)
  (* Bars.run (); *)

  Eio_main.run @@ fun env ->
  Eio.Ctf.with_tracing @@ fun () ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  (* TODO turn this into handler *)
  Url.S.v ~label:"url" ~mgr:env#domain_mgr ~net:env#net |> fun url_s ->

  (* start of app code *)
  let url = Uri.of_string "https://anil.recoil.org/" in
  let bodies = Eio.Fiber.List.map (fun v -> Url.fetch url_s v |> String.length) [ url; url; url; url; url ]  in
  Eio.traceln "bodies: %a" Fmt.(list ~sep:(any ", ") int) bodies
