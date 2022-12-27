open Cohttp_eio

let (/) = Eio.Path.(/)
let ( let* ) = Result.bind

exception Invalid_request_path of string
module Eiox = struct
  let normalise =
    let open Fpath in
    let root = v "/" in fun path ->
    match relativize ~root (v path) with
    | None -> raise (Invalid_request_path path)
    | Some path -> to_string path

  let file_exists f =
    Eio.Switch.run @@ fun sw ->
    try ignore(Eio.Path.open_in ~sw f); true
    with _ -> false
end

module Cohttpx = struct
  let respond_file fname body =
    let fname = snd fname in
    let mime_type = Magic_mime.lookup fname in
    let headers = Http.Header.of_list
      [ "content-type", mime_type;
        "content-length", string_of_int @@ String.length body;
      ] in
    let response =
      Http.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers () in
    response, Body.Fixed body

  let tls_connection_handler ~server_config handler flow client_addr =
    let flow = Tls_eio.server_of_flow server_config flow in
    Cohttp_eio.Server.connection_handler handler flow client_addr

  let run_domain ~server_config ssock handler =
    let on_error exn = Eio.traceln "Error handling connection: %s\n%!" (Printexc.to_string exn) in
    let tls_handler = tls_connection_handler ~server_config handler in
    Eio.Switch.run (fun sw ->
      let rec loop () =
        Eio.Net.accept_fork ~sw ssock ~on_error tls_handler;
        loop ()
      in loop ())

  let tls_serve ?(socket_backlog = 128) ?(domains = 8) ~port ~cert_dir env handler =
    let server_config = 
      let certificate = X509_eio.private_of_pems ~cert:(cert_dir / "certificate.pem") ~priv_key:(cert_dir / "privkey.pem") in
      let certificates = `Multiple [ certificate ] in
      Tls.Config.(server ~version:(`TLS_1_0, `TLS_1_3) ~certificates ~ciphers:Ciphers.supported ()) in
    Eio.Switch.run @@ fun sw ->
    let domain_mgr = Eio.Stdenv.domain_mgr env in
    let ssock =
      Eio.Net.listen env#net ~sw ~reuse_addr:true ~reuse_port:true
      ~backlog:socket_backlog (`Tcp (Eio.Net.Ipaddr.V4.any, port)) in
    for _ = 2 to domains do
      Eio.Std.Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_domain ~server_config ssock handler))
    done;
    run_domain ~server_config ssock handler

end

module LEx = struct

  let main ~priv_pem ~csr_pem ~email ~acme_dir ~endpoint ~cert env =
    let priv_pem = Eio.Path.load (Eio.Path.(Eio.Stdenv.fs env / priv_pem)) in
    let csr_pem = Eio.Path.load (Eio.Path.(Eio.Stdenv.fs env / csr_pem)) in
    let f_exists = Sys.file_exists cert in
    if f_exists then (Eio.traceln "certificate found, skipping LE"; Ok ())
    else begin
      let* account_key = X509.Private_key.decode_pem (Cstruct.of_string priv_pem) in
      let* request = X509.Signing_request.decode_pem (Cstruct.of_string csr_pem) in
      let solver =
        Logs.app (fun m -> m "using http solver, writing to %a" Eio.Path.pp acme_dir);
        let solve_challenge _ ~prefix:_ ~token ~content =
          (* now, resource has .well-known/acme-challenge prepended *)
          let path = Eio.Path.(acme_dir / token) in
          Eio.Path.save ~create:(`Or_truncate 0o600) path content;
          Ok () in
        Letsencrypt.Client.http_solver solve_challenge in
      let sleep n = Eio.Time.sleep env#clock (float_of_int n) in
      let* le = Letsencrypt.Client.initialise env ~endpoint ?email account_key in
      let* certs = Letsencrypt.Client.sign_certificate env solver le sleep request in
      Logs.info (fun m -> m "Certificates downloaded");
      let path = Eio.Path.(Eio.Stdenv.fs env / cert) in
      Eio.Path.save ~create:(`Exclusive 0o600) path (Cstruct.to_string @@ X509.Certificate.encode_pem_multiple certs);
      Ok ()
   end
end

let https_serve ~docroot ~uri path =
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

let http_serve ~tokenroot path =
  (* TODO put a URL router here! *)
  match Fpath.(v path |> segs) with
  | [ ""; ".well-known"; "acme-challenge"; token ] ->
      (* TODO need a streaming responder *)
      let fname = tokenroot / token in
      let body = Eio.Path.load fname in
      Cohttpx.respond_file fname body
  | _ ->
      failwith "TODO redirect to https url"

let http_app _fs ~tokenroot (req, _reader, _client_addr) =
  let uri = Uri.of_string @@ Http.Request.resource req in
  let path = Uri.path uri in
  let meth = Http.Request.meth req in
  Eio.traceln "http %a %s" Http.Method.pp meth path;
  match meth with
  | (`GET|`HEAD) -> http_serve ~tokenroot path
  | _ -> Server.not_found_response

let https_app _fs ~docroot (req, _reader, _client_addr) =
  let uri = Uri.of_string @@ Http.Request.resource req in
  let path = Uri.path uri in
  let meth = Http.Request.meth req in
  Eio.traceln "https %a %s" Http.Method.pp meth path;
  match meth with
  | (`GET|`HEAD) -> https_serve ~docroot ~uri path
  | _ -> Server.not_found_response


let run_http_server ~tokenroot ~port env =
  Eio.traceln "Starting HTTP server";
  Server.run ~domains:2 ~port env (http_app env#fs ~tokenroot)

let run_https_server ~docroot ~cert ~port env =
  Eio.traceln "Starting HTTPS server";
  Cohttpx.tls_serve ~domains:2 ~cert_dir:cert ~port env (https_app env#fs ~docroot)

let main priv_pem csr_pem email acme_dir prod cert () =
  Eio_main.run @@ fun env ->
  Eio.Ctf.with_tracing @@ fun () ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let endpoint = if prod then Letsencrypt.letsencrypt_production_url else Letsencrypt.letsencrypt_staging_url in
  let docroot = Eio.Path.open_dir ~sw (env#cwd / "./site") in
  let tokenroot = Eio.Path.open_dir ~sw (env#cwd / acme_dir) in
  Eio.Fiber.fork ~sw (fun () -> run_http_server ~tokenroot ~port:80 env);
  match LEx.main ~priv_pem ~csr_pem ~email ~acme_dir:tokenroot ~endpoint ~cert env with
  | Error (`Msg m) -> failwith m
  | Ok () -> run_https_server ~docroot ~cert:env#cwd ~port:443 env

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

open Cmdliner

let priv_pem =
  let doc = "File containing the PEM-encoded private key." in
  Arg.(value & opt string "account.pem" & info ["account-key"] ~docv:"FILE" ~doc)

let csr_pem =
  let doc = "File containing the PEM-encoded CSR." in
  Arg.(value & opt string "csr.pem" & info ["csr"] ~docv:"FILE" ~doc)

let acme_dir =
  let doc = "Base path for where to serve and write LetsEncrypt challenges. " in
  Arg.(value & opt string "tokens" & info ["acme-dir"] ~docv:"DIR" ~doc)

let prod =
  let doc = "Production certification generation" in
  Arg.(value & flag & info ["prod"] ~doc)

let cert =
  let doc = "filename where to store the certificate" in
  Arg.(value & opt string "certificate.pem" & info ["cert"] ~doc)

let email =
  let doc = "Contact e-mail for registering new LetsEncrypt keys" in
  Arg.(value & opt (some string) None & info ["email"] ~doc)

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
  let cli = Term.(const main $ priv_pem $ csr_pem $ email $ acme_dir $ prod $ cert $ setup_log) in
  let cmd = Cmd.v info cli in
  exit @@ Cmd.eval cmd
