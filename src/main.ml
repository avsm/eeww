[@@@warning "-32"]

open Cohttp_eio

let ( / ) = Eio.Path.( / )

exception Invalid_request_path of string

module Eiox = struct
  (* UPSTREAM: need a realpath and relative resolver *)
  let normalise =
    let open Fpath in
    let root = v "/" in
    fun path ->
      match relativize ~root (v path) with
      | None -> raise (Invalid_request_path path)
      | Some path -> to_string path

  (* UPSTREAM: need an Eio file exists check without opening *)
  let file_exists f =
    Eio.Switch.run @@ fun sw ->
    try
      ignore (Eio.Path.open_in ~sw f);
      true
    with _ -> false
end

module Cohttpx = struct
  let respond_file fname body =
    let fname = snd fname in
    let mime_type = Magic_mime.lookup fname in
    let headers =
      Http.Header.of_list
        [
          ("content-type", mime_type);
          ("content-length", string_of_int @@ String.length body);
        ]
    in
    let response =
      Http.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ()
    in
    (response, Body.Fixed body)

  let tls_connection_handler ~server_config handler flow client_addr =
    let flow = Tls_eio.server_of_flow server_config flow in
    Cohttp_eio.Server.connection_handler handler flow client_addr

  let run_domain ~server_config ssock handler =
    let on_error exn =
      Eio.traceln "Error handling connection: %s\n%!" (Printexc.to_string exn)
    in
    let tls_handler = tls_connection_handler ~server_config handler in
    Eio.Switch.run (fun sw ->
        let rec loop () =
          Eio.Net.accept_fork ~sw ssock ~on_error tls_handler;
          loop ()
        in
        loop ())

  let tls_serve ?(socket_backlog = 128) ?(domains = 8) ~port ~config env handler
      =
    Eio.Switch.run @@ fun sw ->
    let ssock =
      Eio.Net.listen env#net ~sw ~reuse_addr:true ~reuse_port:true
        ~backlog:socket_backlog
        (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    for _ = 2 to domains do
      Eio.Std.Fiber.fork ~sw (fun () ->
          Eio.Domain_manager.run env#domain_mgr (fun () ->
              run_domain ~server_config:config ssock handler))
    done;
    run_domain ~server_config:config ssock handler
end

let https_serve ~docroot ~uri path =
  (* TODO put a URL router here! *)
  let fname = docroot / Eiox.normalise path in
  Eio.traceln "uri: %a local file: %a, path %s" Uri.pp uri Eio.Path.pp fname
    path;
  Eio.Switch.run (fun sw ->
      let fin = Eio.Path.open_in ~sw fname in
      match Eio.File.((stat fin).kind) with
      | `Directory ->
          let idx = Eio.Path.(fname / "index.html") in
          (* if path doesn't end in /, redirect appending / *)
          if String.length path > 1 && path.[String.length path - 1] != '/' then
            Server.respond_redirect
              ~uri:(Uri.of_string (Uri.to_string uri ^ "/"))
          else if Eiox.file_exists idx then
            Server.html_response (Eio.Path.load idx)
          else Server.html_response "not found"
      | `Regular_file ->
          (* TODO stream body instead of loading *)
          let body = Eio.Path.load fname in
          Cohttpx.respond_file fname body
      | _ -> Server.html_response "not found")

let http_serve path =
  (* TODO put a URL router here! *)
  match Fpath.(v path |> segs) with
  | [ ""; ".well-known"; "acme-challenge"; token ] ->
      Server.text_response (Tls_le.Token_cache.get token)
  | _ -> failwith "TODO redirect to https url"

let http_app (req, _reader, _client_addr) =
  let uri = Uri.of_string @@ Http.Request.resource req in
  let path = Uri.path uri in
  let meth = Http.Request.meth req in
  Eio.traceln "http %a %s" Http.Method.pp meth path;
  match meth with
  | `GET | `HEAD -> http_serve path
  | _ -> Server.not_found_response

let https_app _fs ~docroot (req, _reader, _client_addr) =
  let uri = Uri.of_string @@ Http.Request.resource req in
  let path = Uri.path uri in
  let meth = Http.Request.meth req in
  Eio.traceln "https %a %s" Http.Method.pp meth path;
  match meth with
  | `GET | `HEAD -> https_serve ~docroot ~uri path
  | _ -> Server.not_found_response

let run_http_server ~port env =
  Eio.traceln "Starting HTTP server";
  Server.run ~domains:2 ~port env http_app

let run_https_server ~docroot ~config ~port env =
  Eio.traceln "Starting HTTPS server";
  Cohttpx.tls_serve ~domains:2 ~config ~port env (https_app env#fs ~docroot)

module H2_handler = struct
  open H2

  let request_handler : Eio.Net.Sockaddr.stream -> Reqd.t -> unit =
   fun _client_address request_descriptor ->
    let request = Reqd.request request_descriptor in
    let response_content_type =
      match Headers.get request.headers "Content-Type" with
      | Some request_content_type -> request_content_type
      | None -> "text/plain"
    in
    let response =
      Response.create
        ~headers:(Headers.of_list [ ("content-type", response_content_type) ])
        `OK
    in
    Reqd.respond_with_string request_descriptor response
      "Welcome to an ALPN-negotiated HTTP/2 connection"

  let error_handler :
      Eio.Net.Sockaddr.stream ->
      ?request:H2.Request.t ->
      _ ->
      (Headers.t -> Body.Writer.t) ->
      unit =
   fun _client_address ?request:_ _error start_response ->
    let response_body = start_response Headers.empty in
    Body.Writer.close response_body
end

module H1_handler = struct
  open Httpaf

  let redirect_handler : Eio.Net.Sockaddr.stream -> Reqd.t Gluten.reqd -> unit =
   fun _client_address { Gluten.reqd; _ } ->
    let response =
      Response.create
        ~headers:
          (Headers.of_list
             [ ("Location", "https://localhost:9443"); ("Connection", "close") ])
        `Moved_permanently
    in
    Reqd.respond_with_string reqd response ""

  let redirect_error_handler :
      Unix.sockaddr ->
      ?request:Request.t ->
      _ ->
      (Headers.t -> Body.Writer.t) ->
      unit =
   fun _client_address ?request:_ _error start_response ->
    let response_body = start_response Headers.empty in
    Body.Writer.close response_body

  let has_token s =
     match Scanf.sscanf s "/.well-known/acme-challenge/%s" (fun s -> s) with
     | s -> Some s
     | exception _ -> None

  let request_handler : Eio.Net.Sockaddr.stream -> Reqd.t Gluten.reqd -> unit =
   fun _client_address { Gluten.reqd; _ } ->
    let request = Reqd.request reqd in
    Eio.traceln "HTTP1 %a" Request.pp_hum request;
    let response_content_type =
      match Headers.get request.headers "Content-Type" with
      | Some request_content_type -> request_content_type
      | None -> "text/plain"
    in
    let response_body = 
      match has_token request.target with
      | Some token -> Tls_le.Token_cache.get token
      | None -> "hello alpn world!"
    in
    let response =
      Response.create
        ~headers:
          (Headers.of_list
             [
               ("content-type", response_content_type);
               ("Content-Length", String.length response_body |> string_of_int);
             ])
        `OK
    in
    Reqd.respond_with_string reqd response response_body

  let error_handler :
      Eio.Net.Sockaddr.stream ->
      ?request:Request.t ->
      _ ->
      (Headers.t -> Body.Writer.t) ->
      unit =
   fun _client_address ?request:_ _error start_response ->
    let response_body = start_response Headers.empty in
    Body.Writer.close response_body
end

module Alpn_lib = struct
  let h1_handler flow sa =
    Httpaf_eio.Server.create_connection_handler ?config:None
      ~request_handler:H1_handler.request_handler
      ~error_handler:H1_handler.error_handler sa flow

  let h2_handler =
    H2_eio.Server.create_connection_handler ?config:None
      ~request_handler:H2_handler.request_handler
      ~error_handler:H2_handler.error_handler

  let start_http_server ~sw ~port env =
    Eio.traceln "listening on %d" port;
    let on_error exn =
      Eio.traceln "Connection exn: %s" (Printexc.to_string exn)
    in
    let s =
      Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~reuse_port:true
        ~backlog:10
        (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    while true do
      Eio.traceln "accepting";
      Eio.Net.accept_fork ~sw s ~on_error h1_handler
    done

  let start_https_server ~docroot ~config ~port env =
    let _ = docroot in
    Eio.Switch.run @@ fun sw ->
    let s =
      Eio.Net.listen env#net ~sw ~reuse_addr:true ~reuse_port:true
          ~backlog:10
          (`Tcp (Eio.Net.Ipaddr.V4.any, port))
    in
    let on_error exn =
        Eio.traceln "Error handling TLS connection: %s\n%!" (Printexc.to_string exn)
    in
    while true do
      Eio.Net.accept_fork ~sw s ~on_error (fun flow sa ->
        let flow = Tls_eio.server_of_flow config flow in
        h2_handler sa (flow :> Eio.Flow.two_way)
      )
      done
(*
    let open Lwt.Infix in
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 9443)) in
    let cert = "./certificates/server.pem" in
    let priv_key = "./certificates/server.key" in
    Lwt.async (fun () ->
        Lwt_io.establish_server_with_client_socket listen_address
          (fun client_addr fd ->
            Lwt.catch
              (fun () ->
                X509_lwt.private_of_pems ~cert ~priv_key >>= fun certificate ->
                let config =
                  Tls.Config.server
                    ~alpn_protocols:[ "h2"; "http/1.1" ]
                      (* accept h2 before http/1.1 *)
                    ~certificates:(`Single certificate) ()
                in
                Tls_lwt.Unix.server_of_fd config fd >>= fun tls_server ->
                match Tls_lwt.Unix.epoch tls_server with
                | Error () ->
                    Lwt_io.eprintlf
                      "Unable to fetch session data. Did the handshake fail?"
                | Ok { alpn_protocol; _ } -> (
                    match alpn_protocol with
                    | None ->
                        (* Unable to negotiate a protocol *)
                        Lwt.return_unit
                    | Some "http/1.1" -> http1_handler client_addr tls_server
                    | Some "h2" -> h2_handler client_addr tls_server
                    | _ ->
                        (* Can't really happen - would mean that TLS negotiated a
                         * protocol that we didn't specify. *)
                        assert false))
              (fun exn -> Lwt_io.eprintlf "EXN: %s" (Printexc.to_string exn)))
        >>= fun _server -> Lwt.return_unit);
    let forever, _ = Lwt.wait () in
    forever
    *)
end

let main email org domain prod site cert http_port () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let tls =
    match (email, org, domain) with
    | None, None, None ->
        Eio.traceln "Disabling TLS";
        `No_tls
    | Some email, Some org, Some domain -> `With_tls (email, org, domain)
    | _ ->
        failwith
          "Must specify all of --email, --org, --domain in order to enable TLS"
  in
  let docroot = Eio.Path.open_dir ~sw (env#fs / site) in
  Eio.Fiber.fork ~sw (fun () -> Alpn_lib.start_http_server ~sw ~port:http_port env);
  match tls with
  | `No_tls -> ()
  | `With_tls (email, org, domain) ->
      let endpoint =
        if prod then Letsencrypt.letsencrypt_production_url
        else Letsencrypt.letsencrypt_staging_url
      in
      let cert_root = Eio.Path.open_dir ~sw (env#fs / cert) in
      let config =
        Tls_le.tls_config ~cert_root ~org ~email ~domain ~endpoint env
      in
      Eio.traceln "Starting TLS with LetsEncrypt endpoint: %a" Uri.pp endpoint;
      Alpn_lib.start_https_server ~docroot ~config ~port:443 env

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

open Cmdliner

let prod =
  let doc = "Production certification generation" in
  Arg.(value & flag & info [ "prod" ] ~doc)

let cert =
  let doc = "Directory where to store the certificates" in
  Arg.(value & opt string "certs" & info [ "certs-dir" ] ~doc)

let site =
  let doc = "Directory where to serve the HTML from" in
  Arg.(value & opt string "site" & info [ "html-dir" ] ~doc)

let email =
  let doc = "Contact e-mail for registering new LetsEncrypt keys" in
  Arg.(value & opt (some string) None & info [ "email" ] ~doc)

let org =
  let doc = "Organisation name for the LetsEncrypt keys" in
  Arg.(value & opt (some string) None & info [ "org" ] ~doc)

let domain =
  let doc = "Domain name to issue certificate for" in
  Arg.(value & opt (some string) None & info [ "domain" ] ~doc)

let http_port =
  let doc = "HTTP port to listen on" in
  Arg.(value & opt int 80 & info [ "p"; "http-port" ] ~doc)

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let info =
  let doc = "experimental effects-based webserver" in
  let man =
    [
      `S "DESCRIPTION";
      `P "This is software is experimental.";
      `S "BUGS";
      `P "Email bug reports to <anil@recoil.org>";
    ]
  in
  Cmd.info "eeww" ~version:"%%VERSION%%" ~doc ~man

let () =
  Printexc.record_backtrace true;
  let cli =
    Term.(
      const main $ email $ org $ domain $ prod $ site $ cert $ http_port
      $ setup_log)
  in
  let cmd = Cmd.v info cli in
  exit @@ Cmd.eval cmd
