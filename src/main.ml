open Cohttp_eio

let (/) = Eio.Path.(/)
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
    Eio.traceln "mime: %s %s" fname mime_type;
    let response =
      Http.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers () in
    response, Body.Fixed body
end

let serve ~docroot ~uri path =
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
       Server.html_response "not found"
  )

let app _fs docroot (req, _reader, _client_addr) =
  let uri = Uri.of_string @@ Http.Request.resource req in
  let path = Uri.path uri in
  let meth = Http.Request.meth req in
  Eio.traceln "%a %s" Http.Method.pp meth path;
  match meth with
  | (`GET|`HEAD) -> serve ~docroot ~uri path
  | _ -> Server.not_found_response

let server env =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";
  Eio.Switch.run @@ fun sw ->
  let docroot = Eio.Path.open_dir ~sw (env#cwd / "./site") in
  Server.run ~port:!port env (app env#fs docroot)

let _ =
  Eio_main.run @@ fun env ->
  server env
