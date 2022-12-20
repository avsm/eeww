open Network

let main_queue = Dispatch.Queue.main ()

(* Outbound connection *)
let parameters ?(udp = true) () =
  if udp then Parameters.create () else Parameters.create_tcp ()

let handler (t : Connection.State.t) _ =
  match t with
  | Waiting -> print_endline "Waiting"
  | Ready -> print_endline "Connection Ready"
  | _ -> print_endline "Yikes"

let start_connection connection =
  Connection.set_queue ~queue:main_queue connection;
  Connection.retain connection;
  Connection.set_state_changed_handler ~handler connection;
  Connection.start connection

let outbound hostname port udp =
  let params = parameters ~udp () in
  let endpoint = Endpoint.create_host ~hostname port in
  let connection = Connection.create ~params endpoint in
  connection

(* Inbound Connections *)
let g_inbound_connection : Connection.t option ref = ref None

let inbound ?(hostname = "::") ?(port = 0) () =
  let params = Parameters.create () in
  let endpoint = Endpoint.create_host ~hostname port in
  let _ = Parameters.set_local_endpoint ~endpoint params in
  let _ = Endpoint.release endpoint in
  params

let start_listener params =
  let listener = Listener.create params in
  Listener.set_queue ~queue:main_queue listener;
  Listener.retain listener;
  let handler (t : Listener.State.t) _ =
    match t with Cancelled -> () | _ -> ()
  in
  let conn_handler new_connection =
    match !g_inbound_connection with
    | None ->
        Connection.retain new_connection;
        g_inbound_connection := Some new_connection
    | Some _ -> Connection.cancel new_connection
  in
  Listener.set_state_changed_handler ~handler listener;
  Listener.set_new_connection_handler ~handler:conn_handler listener;
  Listener.start listener;
  listener

(* Receiving & sending *)
let rec receive connection =
  let completion data context _is_complete _err =
    Connection.Context.retain context;
    let schedule_next_receive =
      Dispatch.Block.create (fun () -> receive connection)
    in
    let stdout = Dispatch.Io.create Stream Unix.stdout main_queue in
    Dispatch.Io.with_write ~queue:main_queue ~off:0 ~data
      ~f:(fun ~err:_ ~finished:_ _ -> Dispatch.Block.exec schedule_next_receive)
      stdout
  in
  Connection.receive ~min:1 ~max:max_int ~completion connection

let rec send connection =
  let stdin = Dispatch.Io.create Stream Unix.stdin main_queue in
  Dispatch.Io.with_read ~queue:main_queue ~off:0 ~length:max_int
    ~f:(fun ~err:_ ~finished:_ data ->
      Connection.send ~is_complete:true
        ~context:(Connection.Context.default ())
        ~completion:(fun _ -> send connection)
        ~data connection)
    stdin

(* CLI *)
open Cmdliner

let host =
  let docv = "HOST" in
  let doc = "The host to connect to" in
  Arg.(value & pos 1 string "ocaml.org" & info ~doc ~docv [])

let listen () =
  let params = inbound ~hostname:"localhost" ~port:8080 () in
  let _listener = start_listener params in
  Dispatch.main ()

let listen_info =
  let doc = "listen" in
  Term.info ~doc "listen"

let listen_term = (Term.(pure listen $ pure ()), listen_info)

let run host =
  let conn = outbound host 80 false in
  let _ = start_connection conn in
  let _ = send conn in
  let _ = receive conn in
  Dispatch.main ()

let connect_info =
  let doc = "connect" in
  Term.info ~doc "connect"

let main_term = (Term.(pure run $ host), connect_info)

let cmds = [ main_term; listen_term ]

let doc = "OCaml Netcat"

let main =
  (Term.ret @@ Term.pure (`Help (`Pager, None)), Term.info "ocaml-nc" ~doc)

let main () = Term.(exit @@ eval_choice main cmds)

let () = main ()
