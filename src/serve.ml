open Goose

let serve_file ~docroot ~uri path =
  let open Cohttp_eio in
  (* TODO put a URL router here! *)
  let fname = docroot / (Eiox.normalise path) in
  Eio.traceln "uri: %a local file: %a, path %s" Uri.pp uri Eio.Path.pp fname path;
  Eio.Switch.run (fun sw ->
    let fin = Eio.Path.open_in ~sw fname in
    match Eio.File.((stat fin).kind) with
    |`Directory -> begin
       let idx = Eio.Path.(fname / "index.html") in
       (* if path doesn't end in /, redirect appending / *)
       if String.length path > 1 && path.[(String.length path) - 1] != '/' then
         Server.respond_redirect ~uri:(Uri.of_string ((Uri.to_string uri) ^ "/"))
       else if Eiox.file_exists idx then
         Server.html_response (Eio.Path.load idx)
       else
         Server.html_response "not found"
    end
    |`Regular_file ->
       (* TODO stream body instead of loading *)
       let body = Eio.Path.load fname in
       Cohttpx.respond_file fname body
    | _ ->
       Server.html_response "not found")

let http_serve ~docroot ~uri =
  let path = Uri.path uri in
  let open Cohttp_eio in
  (* TODO put a URL router here! *)
  match Fpath.(v path |> segs) with
  | [ ""; ".well-known"; "acme-challenge"; token ] ->
      Server.text_response (Tls_le.Token_cache.get token)
  | _ -> serve_file ~docroot ~uri path

let http_app ~docroot (req, _reader, _client_addr) =
  let open Cohttp_eio in
  let uri = Uri.of_string @@ Http.Request.resource req in
  let meth = Http.Request.meth req in
  (* Eio.traceln "%a" Http.Request.pp req; *)
  match meth with
  | (`GET|`HEAD) -> http_serve ~docroot ~uri
  | _ -> Server.not_found_response

let on_error exn = Eio.traceln "Error handling connection: %s\n%!" (Printexc.to_string exn) 
let run_http_server ~docroot conn =
  Eio.Switch.run @@ fun sw ->
  Conn.accept ~sw ~on_error conn 
   (Cohttp_eio.Server.connection_handler (http_app ~docroot))

let run_https_server ~docroot tls_s =
  Eio.Switch.run @@ fun sw ->
  Tls_conn.accept ~sw ~on_error tls_s
   (Cohttp_eio.Server.connection_handler (http_app ~docroot))

let main email org domain prod site cert http_port () =
  Eio_main.run @@ fun env ->
  Eio.Ctf.with_tracing @@ fun () ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) ~sleep:(Duration.of_sec 60) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let tls =
    match email, org, domain with
    | None, None, None -> Eio.traceln "Disabling TLS"; `No_tls
    | Some email, Some org, Some domain -> `With_tls (email, org, domain)
    | _ -> failwith "Must specify all of --email, --org, --domain in order to enable TLS"
  in
  let docroot = Eio.Path.open_dir ~sw (env#fs / site) in
  Conn.v ~sw ~label:"conn" ~mgr:env#domain_mgr ~net:env#net ~port:http_port |> fun conn_s ->
  Eio.Fiber.fork ~sw (fun () -> run_http_server ~docroot conn_s);
  match tls with
  | `No_tls -> ()
  | `With_tls (email, org, domain) ->
    let endpoint = if prod then Letsencrypt.letsencrypt_production_url else Letsencrypt.letsencrypt_staging_url in
    let cert_root = Eio.Path.open_dir ~sw (env#fs / cert) in
    let config = Tls_le.tls_config ~cert_root ~org ~email ~domain ~endpoint env in
    Eio.traceln "Starting TLS with LetsEncrypt endpoint: %a" Uri.pp endpoint;
    Conn.v ~sw ~label:"tls_conn" ~mgr:env#domain_mgr ~net:env#net ~port:443 |> fun tcp ->
    Tls_conn.v ~label:"tls" ~mgr:env#domain_mgr ~tcp ~server_config:config |> fun tls_s ->
    run_https_server ~docroot tls_s

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

open Cmdliner

let prod =
  let doc = "Production certification generation" in
  Arg.(value & flag & info ["prod"] ~doc)

let cert =
  let doc = "Directory where to store the certificates" in
  Arg.(value & opt string "certs" & info ["certs-dir"] ~doc)

let site =
  let doc = "Directory where to serve the HTML from" in
  Arg.(value & opt string "site" & info ["html-dir"] ~doc)

let email =
  let doc = "Contact e-mail for registering new LetsEncrypt keys" in
  Arg.(value & opt (some string) None & info ["email"] ~doc)

let org =
  let doc = "Organisation name for the LetsEncrypt keys" in
  Arg.(value & opt (some string) None & info ["org"] ~doc)

let domain =
  let doc = "Domain name to issue certificate for" in
  Arg.(value & opt (some string) None & info ["domain"] ~doc)

let http_port =
  let doc = "HTTP port to listen on" in
  Arg.(value & opt int 80 & info ["p";"http-port"] ~doc)

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let info =
  let doc = "experimental effects-based webserver" in
  let man = [
      `S "DESCRIPTION"; `P "This is software is experimental.";
      `S "BUGS"; `P "Email bug reports to <anil@recoil.org>";
    ] in
  Cmd.info "eeww" ~version:"%%VERSION%%" ~doc ~man

let () =
  Printexc.record_backtrace true;
  let cli = Term.(const main $ email $ org $ domain $ prod $ site $ cert $ http_port $ setup_log) in
  let cmd = Cmd.v info cli in
  exit @@ Cmd.eval cmd
