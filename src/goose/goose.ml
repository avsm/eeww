open Eio

module Conn : sig
  val conn : unit Domain_manager.handle
  type t
  val v : sw:Eio.Switch.t -> label:string -> mgr:Domain_manager.t -> 
          net:#Net.t -> port:int -> t
  val accept : sw:Eio.Switch.t -> on_error:(exn -> unit) -> t ->
     Eio.Net.connection_handler -> unit
end = struct

  module S = struct
    type t = {
      label: string;
      mgr : Eio.Domain_manager.t;
      sock: Eio.Net.listening_socket;
      active: int Atomic.t;
    } [@@warning "-69"]

    let v ~label ~mgr ~sock =
       traceln "created Conn %s" label;
       { label; mgr; sock; active = Atomic.make 0 }

    let _pp fmt t = Fmt.str fmt "Conn: %s" t.label
  end

  type _ Effect.t += Conn : unit Effect.t
  [@@warning "-38"] (* TODO dummy effect, will remove *)

  let handler =
    let open Effect.Deep in
    {
      retc = (fun v -> v);
      exnc = raise;
      effc = (fun (type a) (e : a Effect.t) -> match e with
          | Conn -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None
        );
    }

  let conn = Eio.Domain_manager.register_handler handler
  type t = S.t
  let task t ts = Eio.Domain_manager.submit t.S.mgr conn ts
  let accept ~sw ~on_error conn handler =
    while true do
      Net.accept_fork ~sw conn.S.sock ~on_error 
        (fun flow client_addr ->
           Atomic.incr conn.S.active;
           Eio.traceln "accepted: %a active=%d" Eio.Net.Sockaddr.pp client_addr (Atomic.get conn.S.active);
           let () =
             try
               let fn () = handler flow client_addr in
               task conn fn;
               Eio.traceln "done task";
             with exn -> (
               Eio.traceln "tcp exn: %s" (Printexc.to_string exn);
               Eio.traceln "done task with exn")
           in
           Atomic.decr conn.S.active;
           Eio.traceln "closed: %a active=%d" Eio.Net.Sockaddr.pp client_addr (Atomic.get conn.S.active)
        )
    done
  
  let v ~sw ~label ~mgr ~net ~port =
    let sock =
      Net.listen net ~sw ~reuse_addr:true ~reuse_port:true
        ~backlog:100 (`Tcp (Eio.Net.Ipaddr.V4.any, port))
        (* TODO backlog number must go *)
    in
    traceln "Conn: listened on port %d" port;
    S.v ~label ~mgr ~sock

  let _pp fmt t = Fmt.str fmt "%s" t.S.label
end

module Tls_conn : sig
  val conn : unit Domain_manager.handle
  type t
  val v : label:string -> mgr:Domain_manager.t -> 
    tcp:Conn.t -> server_config:Tls.Config.server -> t
  val accept : sw:Eio.Switch.t -> on_error:(exn -> unit) -> t ->
     (Tls_eio.t -> Eio.Net.Sockaddr.stream -> unit) -> unit
end = struct

  module S = struct
    type t = {
      label: string;
      mgr : Eio.Domain_manager.t;
      tcp : Conn.t;
      server_config : Tls.Config.server;
      active : int Atomic.t;
    } [@@warning "-69"]

    let v ~label ~mgr ~tcp ~server_config =
       traceln "created Tls_conn %s" label;
       { label; mgr; tcp; server_config; active = Atomic.make 0 }

    let _pp fmt t = Fmt.str fmt "Tls_conn: %s" t.label
  end

  type _ Effect.t += Conn : unit Effect.t
  [@@warning "-38"] (* TODO dummy effect, will remove *)

  let handler =
    let open Effect.Deep in
    {
      retc = (fun v -> v);
      exnc = raise;
      effc = (fun (type a) (e : a Effect.t) -> match e with
          | Conn -> Some (fun (k : (a, _) continuation) -> continue k ())
          | _ -> None
        );
    }

  let conn = Eio.Domain_manager.register_handler handler
  type t = S.t
  let task t ts = Eio.Domain_manager.submit t.S.mgr conn ts

  let accept ~sw ~on_error conn handler =
    let server_config = conn.S.server_config in
    let tcp_conn = conn.S.tcp in
    while true do
       Conn.accept ~sw ~on_error tcp_conn (fun flow client_addr ->
         let fn () =
           Atomic.incr conn.S.active;
           Eio.traceln "Tls: start active=%d" (Atomic.get conn.S.active);
           let () =
             try
               let flow = Tls_eio.server_of_flow server_config flow in
               handler flow client_addr 
             with exn ->
                Eio.traceln "tls exn: %s" (Printexc.to_string exn)
           in
           Atomic.decr conn.S.active;
           Eio.traceln "Tls: close active=%d" (Atomic.get conn.S.active)
         in
         task conn fn
       )
    done

  let v ~label ~mgr ~tcp ~server_config =
    traceln "Tls_conn: listening";
    S.v ~label ~mgr ~tcp ~server_config

  let _pp fmt t = Fmt.str fmt "%s" t.S.label
end


let (/) = Eio.Path.(/)

exception Invalid_request_path of string
module Eiox = struct
  (* UPSTREAM: need a realpath and relative resolver *)
  let normalise =
    let open Fpath in
    let root = v "/" in fun path ->
    match relativize ~root (v path) with
    | None -> raise (Invalid_request_path path)
    | Some path -> to_string path

  (* UPSTREAM: need an Eio file exists check without opening *)
  let file_exists f =
    Eio.Switch.run @@ fun sw ->
    try ignore(Eio.Path.open_in ~sw f); true
    with _ -> false
end

module Cohttpx = struct
  open Cohttp_eio
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
end
