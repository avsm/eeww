module Flow = Eio.Flow

type error  = [ `Tls_alert   of Tls.Packet.alert_type
              | `Tls_failure of Tls.Engine.failure
              | `Read of exn
              | `Write of exn ]

type write_error = [ `Closed | error ]
(** The type for write errors. *)

let pp_error ppf = function
  | `Tls_failure f -> Fmt.string ppf @@ Tls.Engine.string_of_failure f
  | `Tls_alert a   -> Fmt.string ppf @@ Tls.Packet.alert_type_to_string a
  | `Read ex       -> Fmt.exn ppf @@ ex
  | `Write ex      -> Fmt.exn ppf @@ ex

let pp_write_error ppf = function
  | #error as e                   -> pp_error ppf e
  | `Closed -> Fmt.string ppf "Closed"

type flow = {
  role           : [ `Server | `Client ];
  flow           : Flow.two_way;
  mutable state  : [ `Active of Tls.Engine.state
                   | `Eof
                   | `Error of error ];
  mutable linger : Cstruct.t list;
}

let tls_alert a = `Error (`Tls_alert a)
let tls_fail f  = `Error (`Tls_failure f)

let lift_read_result = function
  | Ok (`Data _ | `Eof as x) -> x
  | Error e                  -> `Error (`Read e)

let lift_write_result = function
  | Ok ()   -> `Ok ()
  | Error e -> `Error (`Write e)

let check_write flow f_res =
  let res = lift_write_result f_res in
  ( match flow.state, res with
    | `Active _, (`Eof | `Error _ as e) ->
        flow.state <- e ; Flow.shutdown flow.flow `All
    | _ -> () );
  match f_res with
  | Ok ()   -> Ok ()
  | Error e -> Error (`Write e :> write_error)

let copy src dst =
  match Flow.copy src dst with
  | () -> Ok ()
  | exception ex -> Error ex

let read_react flow =
  match flow.state with
  | `Eof | `Error _ as e -> e
  | `Active _            ->
    let cbuf = Cstruct.create 4096 in
    match Flow.read flow.flow cbuf with
    | exception (End_of_file as ex) -> flow.state <- `Eof; raise ex
    | exception ex -> flow.state <- `Error (`Read ex); raise ex
    | got ->
      match flow.state with
      | `Eof | `Error _ as e -> e
      | `Active tls          ->
        let cbuf = Cstruct.sub cbuf 0 got in
        match Tls.Engine.handle_tls tls cbuf with
        | Ok (res, `Response resp, `Data data) ->
          flow.state <- ( match res with
              | `Ok tls      -> `Active tls
              | `Eof         -> `Eof
              | `Alert alert -> tls_alert alert );
          let _ =
            match resp with
            | None     -> Ok ()
            | Some buf -> copy (Flow.cstruct_source [buf]) flow.flow |> check_write flow
          in
          ( match res with
            | `Ok _ -> ()
            | _     -> Flow.shutdown flow.flow `All);
          `Ok data
        | Error (fail, `Response resp) ->
          let reason = tls_fail fail in
          flow.state <- reason ;
          begin try Flow.copy (Flow.cstruct_source [resp]) flow.flow with _ -> () end;
          Flow.shutdown flow.flow `All;
          reason

let rec read_into t buf =
  let got, bufs = Cstruct.fillv ~src:t.linger ~dst:buf in
  t.linger <- bufs;
  if got > 0 then got
  else (
    read_react t |> function
    | `Ok None       -> read_into t buf
    | `Ok (Some next) ->
      t.linger <- t.linger @ [next];
      read_into t buf
    | `Eof           -> raise End_of_file
    | `Error (`Read ex | `Write ex) -> raise ex
    | `Error e       -> raise (Failure (Fmt.to_to_string pp_error e))
  )

let writev flow bufs =
  match flow.state with
  | `Eof     -> Error `Closed
  | `Error e -> Error (e :> write_error)
  | `Active tls ->
      match Tls.Engine.send_application_data tls bufs with
      | Some (tls, answer) ->
          flow.state <- `Active tls ;
          copy (Flow.cstruct_source [answer]) flow.flow |> check_write flow
      | None ->
          (* "Impossible" due to handshake draining. *)
          assert false

let write flow buf = writev flow [buf]

let close flow =
  match flow.state with
  | `Active tls ->
    flow.state <- `Eof ;
    let (_, buf) = Tls.Engine.send_close_notify tls in
    (* XXX: need a switch here *)
    copy (Flow.cstruct_source [buf]) flow.flow |> fun _ -> Flow.shutdown flow.flow `All
  | _           -> ()

(*
 * XXX bad XXX
 * This is a point that should particularly be protected from concurrent r/w.
 * Doing this before a `t` is returned is safe; redoing it during rekeying is
 * not, as the API client already sees the `t` and can mistakenly interleave
 * writes while this is in progress.
 * *)
let rec drain_handshake flow =
  match flow.state with
  | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> ()
  | _ ->
    (* read_react re-throws *)
      read_react flow |> function
      | `Ok mbuf ->
          flow.linger <- Option.to_list mbuf @ flow.linger ;
          drain_handshake flow
      | `Error e -> raise (Failure (Fmt.to_to_string pp_write_error e))
      | `Eof     -> raise End_of_file

let wrap flow =
  object (_ : <Eio.Generic.t; Flow.source; Flow.sink; ..>)
    method probe _ = None

    method read_into buf = read_into flow buf

    method read_methods = []  (* TODO: this would be faster *)

    method copy src =
      let buf = Cstruct.create 4096 in
      let got = Flow.read src buf in (* XXX: wrap errors? *)
      match write flow (Cstruct.sub buf 0 got) with
      | Ok () -> ()
      | Error `Closed -> raise End_of_file
      | Error (`Read ex | `Write ex) -> raise ex
      | Error err -> raise (Failure (Fmt.to_to_string pp_write_error err))

    method epoch =
      match flow.state with
      | `Eof | `Error _ -> Error ()
      | `Active tls     ->
          match Tls.Engine.epoch tls with
          | `InitialEpoch -> assert false (* `drain_handshake` invariant. *)
          | `Epoch e      -> Ok e

    method reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) () =
      match flow.state with
      | `Eof        -> raise End_of_file
      | `Error e    -> raise (Failure (Fmt.to_to_string pp_write_error e))
      | `Active tls ->
          match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
          | None             ->
              (* XXX make this impossible to reach *)
              invalid_arg "Renegotiation already in progress"
          | Some (tls', buf) ->
              if drop then flow.linger <- [] ;
              flow.state <- `Active tls' ;
              copy (Flow.cstruct_source [buf]) flow.flow |> fun _ ->
              drain_handshake flow

    method key_update ?request () =
      match flow.state with
      | `Eof        -> Error `Closed
      | `Error e    -> Error (e :> write_error)
      | `Active tls ->
        match Tls.Engine.key_update ?request tls with
        | Error _ -> invalid_arg "Key update failed"
        | Ok (tls', buf) ->
          flow.state <- `Active tls' ;
          copy (Flow.cstruct_source [buf]) flow.flow |> check_write flow

    method close = close flow
    method shutdown _ = close flow    (* XXX *)
  end

let client_of_flow conf ?host flow =
  let conf' = match host with
    | None      -> conf
    | Some host -> Tls.Config.peer conf host
  in
  let (tls, init) = Tls.Engine.client conf' in
  let tls_flow = {
    role   = `Client ;
    flow   = flow ;
    state  = `Active tls ;
    linger = [] ;
  } in
  copy (Flow.cstruct_source [init]) flow |> fun _ ->
  drain_handshake tls_flow;
  wrap tls_flow

let server_of_flow conf flow =
  let tls_flow = {
    role   = `Server ;
    flow   = flow ;
    state  = `Active (Tls.Engine.server conf) ;
    linger = [] ;
  } in
  drain_handshake tls_flow;
  wrap tls_flow
