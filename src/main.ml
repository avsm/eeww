[@@@warning "-32"]

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
     let fin = Eio.Path.open_in ~sw f in
     match Eio.File.((stat fin).kind) with
     | `Regular_file -> `Regular_file
     | `Directory -> `Directory
     | _ -> `Not_found
    with _ -> `Not_found

end

module H2_handler = struct
  open H2

  let respond_with_file reqd fname =
    let body = Eio.Path.load fname in
    let mime_type = Magic_mime.lookup (snd fname) in
    let response =
      Response.create
        ~headers:(Headers.of_list [ ("content-type", mime_type) ])
        `OK
    in
    Reqd.respond_with_string reqd response body

  let respond_redirect reqd uri =
    let response =
      Response.create 
        ~headers:(Headers.of_list [ ("location", uri) ])
        `Moved_permanently
    in
    Reqd.respond_with_string reqd response ""

  let request_handler ~docroot _ca reqd =
    let request = Reqd.request reqd in
    let path = request.target in
    let fname = docroot / Eiox.normalise path in
    Eio.traceln "local file: %a path %s" Eio.Path.pp fname path;
      match Eiox.file_exists fname with
      | `Directory ->
          let idx = Eio.Path.(fname / "index.html") in
          if String.length path > 1 && path.[String.length path - 1] != '/' then
            respond_redirect reqd (path ^ "/")
          else if Eiox.file_exists idx = `Regular_file then
            respond_with_file reqd idx
          else Reqd.respond_with_string reqd (Response.create `Not_found) "Not found"
      | `Regular_file ->
          (* TODO stream body instead of loading *)
          respond_with_file reqd fname
      | `Not_found ->
          Reqd.respond_with_string reqd (Response.create `Not_found) "Not found"

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

  let h2_handler ~docroot flow sa =
    H2_eio.Server.create_connection_handler ?config:None
      ~request_handler:(H2_handler.request_handler ~docroot)
      ~error_handler:H2_handler.error_handler sa flow

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
        match Tls_eio.epoch flow with
        | Error () -> failwith "ALPN negotiation failed"
        | Ok {alpn_protocol;_} ->
            match alpn_protocol with
            | None -> ()
            | Some "http/1.1" -> h1_handler (flow :> Eio.Flow.two_way) sa
            | Some "h2" -> h2_handler ~docroot (flow :> Eio.Flow.two_way) sa
            | Some _ -> Eio.traceln "invalid ALPN response"; ()
      )
    done
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
        Tls_le.tls_config ~alpn_protocols:[ "h2"; "http/1.1" ]
          ~cert_root ~org ~email ~domain ~endpoint env
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
