(*
 * Copyright (C) 2021 Thomas Leonard
 * Copyright (C) 2021 Patrick Ferris
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Eio.Std
open Eio.Private
open Eio_utils

module Ctf = Eio.Private.Ctf

module PendingIO = struct
  type t = int Atomic.t

  let enter msg t =
    Atomic.incr t

  let leave msg t =
    Atomic.decr t

  let is_finished t = Int.equal (Atomic.get t) 0

  let create () = Atomic.make 0
end

(* SIGPIPE makes no sense in a modern application. *)
let () = Sys.(set_signal sigpipe Signal_ignore)

module Buffer = struct
  type t = Dispatch.Data.t ref
  let empty () = ref @@ Dispatch.Data.empty ()

  (* let set buff buff' = buff := buff' *)

  let concat buff buff' = buff := Dispatch.Data.concat !buff buff'

  let of_bigarray buf = ref @@ Dispatch.Data.create buf

  let _to_string buff =
    let buff = !buff in
    Cstruct.(to_string @@ of_bigarray @@ (Dispatch.Data.to_buff ~offset:0 (Dispatch.Data.size buff) buff))
end

type runnable =
  | IO
  | Thread of string * (unit -> [`Exit_scheduler])

type t = {
  name : string;
  async : Dispatch.Queue.t;       (* Will process [run_q] when prodded. *)
  run_q : runnable Lf_queue.t;
  need_wakeup : bool Atomic.t;
  pending_io : PendingIO.t;  (* All IO operations enter and leave this PendingIO. *)
  wakeup : Dispatch.Semaphore.t;
  wakeup_lock : Mutex.t;
}

type _ Effect.t += Enter : (t -> 'a Suspended.t -> unit) -> 'a Effect.t

let enter fn = Effect.perform (Enter fn)


let wake_buffer =
  let b = Bytes.create 8 in
  Bytes.set_int64_ne b 0 1L;
  b

let option_get ~msg = function
  | Some v -> v
  | None -> failwith ("Option is None: " ^ msg)

let wakeup t =
  Atomic.set t.need_wakeup false; (* [t] will check [run_q] after getting the event below *)
  Dispatch.Semaphore.signal t.wakeup

let enqueue_thread ?(leave_io=false) ~msg t k v =
  Lf_queue.push t.run_q (Thread (msg, fun () -> Suspended.continue k v));
  Mutex.lock t.wakeup_lock;
  if Atomic.get t.need_wakeup then begin
    wakeup t;
    if leave_io then PendingIO.leave msg t.pending_io;
    Mutex.unlock t.wakeup_lock;
  end else (
    if leave_io then PendingIO.leave msg t.pending_io;
    Mutex.unlock t.wakeup_lock
  )

let enqueue_failed_thread ?(leave_io=false) ~msg t k ex =
  Lf_queue.push t.run_q (Thread (msg, fun () -> Suspended.discontinue k ex));
  Mutex.lock t.wakeup_lock;
  if Atomic.get t.need_wakeup then begin
    wakeup t;
    if leave_io then PendingIO.leave msg t.pending_io;
    Mutex.unlock t.wakeup_lock
  end else (
    if leave_io then PendingIO.leave msg t.pending_io;
    Mutex.unlock t.wakeup_lock
  )

let set_cancel_fn fiber f =
  let cancelled = ref false in
  Fiber_context.set_cancel_fn fiber (fun ex -> f ex cancelled);
  (fun () -> Fiber_context.clear_cancel_fn fiber; not !cancelled)

module Low_level = struct

  let sleep_ns ~queue delay =
    enter @@ fun t k ->
    PendingIO.enter "sleepns" t.pending_io;
    (* TODO: Dispatch work items are properly cancellable! *)
    let clear_cancel = set_cancel_fn k.fiber (fun ex cancelled ->
      cancelled := true;
      enqueue_failed_thread ~leave_io:true ~msg:"Timeout stopped!" t k ex
      )
    in
    Dispatch.after ~delay queue (fun () ->
        match Fiber_context.get_error k.fiber with
        | None ->
          if clear_cancel () then begin
            enqueue_thread ~leave_io:true ~msg:"Sleepy" t k ()
          end
        | Some exn ->
          if clear_cancel () then begin
            enqueue_failed_thread ~leave_io:true ~msg:"Timeout stopped!" t k exn
          end
      )

  (* A read can call the callback multiple times with more and more chunks of
     data -- here we concatenate them and only once the read is finished reading
     do we enqueue thread. *)
  let read ~off ~length fd io_queue (buf : Buffer.t) =
    let read = ref 0 in
    enter (fun t k ->
        PendingIO.enter "read" t.pending_io;
        let clear_cancel = set_cancel_fn k.fiber (fun _ cancelled ->
            cancelled := true;
            enqueue_failed_thread ~leave_io:true ~msg:"cancelled" t k (Eio.Cancel.Cancelled Exit)
          )
        in
        Dispatch.Io.with_read ~off ~length ~queue:io_queue fd ~f:(fun ~err ~finished r ->
            let size = Dispatch.Data.size r in
            if err = 0 && finished then begin
              if size = 0 then begin
                if clear_cancel () then begin
                  enqueue_thread ~leave_io:true ~msg:"read of zero" t k !read
                end
              end
              else (
                read := !read + size;
                buf := Dispatch.Data.concat r !buf;
                if clear_cancel () then begin
                  enqueue_thread ~leave_io:true ~msg:("read not zero: " ^ string_of_int size) t k !read
                end
              )
            end
            else if err <> 0 then begin
              if clear_cancel () then begin
                enqueue_failed_thread ~leave_io:true ~msg:"failed read" t k (Failure "Read failed")
              end
            end
            else begin
              read := !read + size;
              buf := Dispatch.Data.concat r !buf
            end
          )
      )

  let write fd io_queue (bufs : Buffer.t) =
    enter (fun t k ->
        PendingIO.enter "write" t.pending_io;
        let clear_cancel = set_cancel_fn k.fiber (fun _ cancelled ->
            cancelled := true;
            enqueue_failed_thread ~leave_io:true ~msg:"cancelled" t k (Eio.Cancel.Cancelled Exit)
          )
        in
        Dispatch.Io.with_write ~off:0 ~data:(!bufs) ~queue:io_queue fd ~f:(fun ~err ~finished _remaining ->
            if err <> 0 then begin
              if clear_cancel () then begin
                enqueue_failed_thread ~leave_io:true ~msg:"failed write" t k (Failure "Write failed");
              end
            end else
            if finished then begin
              if clear_cancel () then begin
                enqueue_thread ~leave_io:true ~msg:"write finished" t k ()
              end
            end
            else ()
          )
      )
end

module File = struct
  type t = {
    mutable release_hook : Eio.Switch.hook;        (* Use this on close to remove switch's [on_release] hook. *)
    mutable fd : [`Open of Dispatch.Io.t | `Closed]
  }

  let get op = function
    | { fd = `Open fd; _ } -> fd
    | { fd = `Closed ; _ } -> invalid_arg (op ^ ": file descriptor used after calling close!")

  let is_open = function
    | { fd = `Open _; _ } -> true
    | { fd = `Closed; _ } -> false

  let close t =
    Ctf.log "close";
    let fd = get "close" t in
    t.fd <- `Closed;
    Eio.Switch.remove_hook t.release_hook;
    Dispatch.Io.close fd

  let ensure_closed t =
    if is_open t then close t

  let _to_gcd = get "to_gcd"

  let of_gcd_no_hook fd =
    { fd = `Open fd; release_hook = Eio.Switch.null_hook }

  let of_gcd ~sw fd =
    let t = of_gcd_no_hook fd in
    t.release_hook <- Switch.on_release_cancellable sw (fun () -> ensure_closed t);
    t

  let to_unix op t =
    let io_channel = get "to_unix" t in
    let fd = Dispatch.Io.get_unix io_channel |> option_get ~msg:"to_unix" in
    match op with
    | `Peek -> fd
    | `Take ->
      t.fd <- `Closed;
      Eio.Switch.remove_hook t.release_hook;
      fd


  let realpath p = try Ok (Unix.realpath p) with exn -> Error exn

  let mkdir ~mode p = try Ok (Unix.mkdir p mode) with exn -> Error exn

  (* We use `Unix.openfile` here because dispatch tries to be clever otherwise and will not open a
     file descriptor until the first IO operation. *)
  let open_ ~sw ?mode io_queue path flags =
    let create path =
      try
        let fd = Unix.openfile path flags (Option.value ~default:0 mode) in
        let io_channel = Dispatch.Io.create Stream (Dispatch.Io.Fd.of_unix fd) io_queue in
        Ok (of_gcd ~sw io_channel)
      with
      | exn -> Error exn
    in
    match Filename.is_relative path with
    | false -> create path
    | true -> match realpath Filename.current_dir_name with
      | Ok parent -> create Filename.(concat parent path)
      | Error _ as e -> e

  let read ~off ~length io_queue fd = Low_level.read ~off ~length (get "with_read" fd) io_queue
  let write io_queue fd = Low_level.write (get "with_read" fd) io_queue
end

module Conn = struct
  let receive ~max (conn : Network.Connection.t) buf =
    let r = enter (fun t k ->
        PendingIO.enter "receive" t.pending_io;
        let clear_cancel = set_cancel_fn k.fiber (fun exn cancelled ->
            (* Can't see any way to actually cancel the receive operation. *)
            cancelled := true;
            enqueue_failed_thread ~leave_io:true ~msg:"Failed network receive" t k exn
          )
        in
        Network.Connection.receive ~min:0 ~max conn ~completion:(fun data _context is_complete err ->
            let err_code = Network.Error.to_int err in
            match data with
            | None ->
              if clear_cancel () then begin
                if err_code <> 0 then
                  enqueue_failed_thread ~leave_io:true ~msg:"rev" t k (Failure (string_of_int err_code))
                else
                  enqueue_thread ~leave_io:true ~msg:"recv no data" t k (Ok (0, true))
              end
            | Some data ->
              let res =
                if err_code = 0 then begin
                  let size = Dispatch.Data.size data in
                  Buffer.concat buf data;
                  Ok (size, is_complete)
                end else Error (`Msg (string_of_int err_code))
              in
              if clear_cancel () then begin
                enqueue_thread ~leave_io:true ~msg:"recv data" t k res
              end
          )
      ) in match r with
    | Ok (0, true) -> raise End_of_file
    | Ok (got, true) -> got
    | Ok (got, false) -> got
    | Error (`Msg e) -> failwith ("Connection receive failed with " ^ e)

  let _receive_message (conn : Network.Connection.t) buf =
    let r = enter (fun t k ->
        PendingIO.enter "recv" t.pending_io;
        let clear_cancel = set_cancel_fn k.fiber (fun exn cancelled ->
            (* Can't see any way to actually cancel the receive operation. *)
            cancelled := true;
            enqueue_failed_thread ~leave_io:true ~msg:"Failed network receive" t k exn
          )
        in
        Network.Connection.receive_message conn ~completion:(fun data _context is_complete err ->
            match data with
            | None ->
              if clear_cancel () then begin
                enqueue_thread ~leave_io:true ~msg:"recv no data" t k (Ok (0, true))
              end
            | Some data ->
              let err_code = Network.Error.to_int err in
              let res =
                if err_code = 0 then begin
                  let size = Dispatch.Data.size data in
                  Buffer.concat buf data;
                  Ok (size, is_complete)
                end else Error (`Msg (string_of_int err_code))
              in
              if clear_cancel () then begin
                enqueue_thread ~leave_io:true ~msg:"recv data" t k res
              end
          )
      ) in match r with
    | Ok (0, true) -> raise End_of_file
    | Ok (got, true) -> got
    | Ok (got, false) -> got
    | Error (`Msg e) -> failwith ("Connection receive failed with " ^ e)

  let send ?(is_complete=true) ?(context=Network.Connection.Context.Default) (conn : Network.Connection.t) buf =
    enter (fun t k ->
        PendingIO.enter "send" t.pending_io;
        let clear_cancel = set_cancel_fn k.fiber (fun exn cancelled ->
            (* Can't see any way to actually cancel the receive operation. *)
            cancelled := true;
            enqueue_failed_thread ~leave_io:true ~msg:"Failed network receive" t k exn
          )
        in
        Network.Connection.send ~is_complete ~context conn ~data:(!buf) ~completion:(fun e ->
            match Network.Error.to_int e with
            | 0 ->
              if clear_cancel () then begin
                enqueue_thread ~leave_io:true ~msg:"sent" t k (Ok ())
              end
            | i ->
              if clear_cancel () then begin
                enqueue_thread ~leave_io:true ~msg:"send err" t k (Error (`Msg (string_of_int i)))
              end
          )
      )

end

let socket sock = object
  inherit Eio.Flow.two_way

  (* Lots of copying :/ *)
  method read_into buff =
    let data = Buffer.empty () in
    let res = Conn.receive ~max:(Cstruct.length buff) sock data in
    let cs = Cstruct.of_bigarray @@ Dispatch.Data.to_buff ~offset:0 res !data in
    let size = min (Cstruct.length buff) res in
    Cstruct.blit cs 0 buff 0 size;
    match size with
    | 0 -> raise End_of_file
    | got -> got

  method copy src =
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Eio.Flow.single_read src buf in
        let data = Buffer.of_bigarray @@ Cstruct.(to_bigarray (sub buf 0 got)) in
        match Conn.send sock data with
        | Ok () -> ()
        | Error (`Msg m) -> failwith m
      done
    with End_of_file -> ()

  method close =
    Network.Connection.cancel sock

  method shutdown = function
    | `Send -> (
        (* Deep in a connection.h file it says:
            "In order to close a connection on the sending side (a "write close"), send
            a context that is marked as "final" and mark is_complete. The convenience definition
            NW_CONNECTION_FINAL_MESSAGE_CONTEXT may be used to define the default final context
            for a connection." *)
        let r = enter (fun t k ->
            Network.Connection.send ~is_complete:true ~context:Final sock ~completion:(fun e ->
                match Network.Error.to_int e with
                | 0 ->
                  enqueue_thread ~leave_io:true ~msg:"shutdown write" t k (Ok ())
                | i ->
                  enqueue_thread ~leave_io:true ~msg:"shutdown err" t k (Error (`Msg (string_of_int i)))
              )
          ) in match r with
        | Ok () -> ()
        | Error (`Msg m) -> failwith m
      )
    | `Receive -> failwith "shutdown receive not supported"
    | `All -> ()
end

let udp_socket local_addr (listener : Network.Listener.t) = object
  inherit Eio.Net.datagram_socket

  val queue = Dispatch.Queue.create ~typ:Serial ()

  val stream = Eio.Stream.create max_int

  (* More copying :/ *)
  method recv buff =
    let size, addr, buf = Eio.Stream.take stream in
    let data =
      Cstruct.of_bigarray @@ Dispatch.Data.to_buff ~offset:0 size !buf
    in
    Cstruct.blit data 0 buff 0 size;
    addr, size

  method send (addr : Eio.Net.Sockaddr.datagram) buf =
    let open Network in
    let ip, port = match addr with
      | `Udp v -> v
    in
    let local_endpoint = match local_addr with
      | `Udp (ip, port) ->
        Some (Endpoint.create_address Unix.(ADDR_INET (Eio_unix.Ipaddr.to_unix ip, port)))
      | _ -> None
    in
    let params = Parameters.create_udp () in
    let endpoint = Endpoint.create_address Unix.(ADDR_INET (Eio_unix.Ipaddr.to_unix ip, port)) in
    Option.iter (fun endpoint -> Parameters.set_local_endpoint ~endpoint params) local_endpoint;
    Parameters.set_reuse_local_address params true;
    let connection = Connection.create ~params endpoint in
    let _ = Endpoint.release endpoint in
    Connection.retain connection;
    Connection.set_queue ~queue connection;
    let handler t k (state : Network.Connection.State.t) e =
      if Network.Error.to_int e <> 0 then enqueue_failed_thread ~msg:"connection" t k (Failure "UDP Connection")
      else
        match state with
        | Waiting -> ()
        | Ready -> enqueue_thread ~msg:"ready connection" t k connection
        | Invalid -> ()
        | Preparing -> ()
        | Failed -> ()
        | Cancelled -> ()
    in
    let connection =
      enter (fun t k ->
          Connection.set_state_changed_handler ~handler:(handler t k) connection;
          Connection.start connection)
    in
    let buff = Buffer.of_bigarray (Cstruct.to_bigarray buf) in
    Conn.send ~is_complete:true ~context:Final connection buff |> Result.get_ok;
    Connection.cancel connection

  method close =
    Network.Listener.cancel listener

  initializer
    let open Network in
    let handler conn =
      Connection.retain conn;
      Connection.set_queue ~queue conn;
      let endpoint = Connection.copy_endpoint conn in
      let sockaddr = match Option.get @@ Endpoint.get_address endpoint with
        | Unix.ADDR_INET (host, port) -> `Udp (Eio_unix.Ipaddr.of_unix host, port)
        | _ -> assert false
      in
      let handler (state : Network.Connection.State.t) _ =
        match state with
        | Waiting -> ()
        | Ready -> ()
        | Invalid -> ()
        | Preparing -> ()
        | Failed -> ()
        | Cancelled -> ()
      in
      Connection.set_state_changed_handler ~handler conn;
      Connection.start conn;
      (* We don't want to actually block here by using Conn.receive, this
         will just push datagrams into the stream as an when they arrive. *)
      let buff = Buffer.empty () in
      Network.Connection.receive_message conn ~completion:(fun data _context _is_complete err ->
          match data with
          | None -> Eio.Stream.add stream (0, sockaddr, Buffer.empty ())
          | Some data ->
            let err_code = Network.Error.to_int err in
            if err_code = 0 then begin
              let size = Dispatch.Data.size data in
              Buffer.concat buff data;
              Eio.Stream.add stream (size, sockaddr, buff)
            end else failwith "Error receiving data on UDP socket"
        )
    in
    let state_handler t k (state : Network.Listener.State.t) err =
      match (Network.Error.to_int err, state) with
      | i, _ when i <> 0 -> failwith ("Listener failed with error code: " ^ string_of_int i)
      | _, Ready -> enqueue_thread ~msg:"UDP listener ready" t k ()
      | i, Failed -> failwith ("Network listener failed: " ^ string_of_int i)
      | _, Invalid -> ()
      | _, Waiting -> ()
      | _, Cancelled -> ()
    in
    Listener.retain listener;
    Listener.set_queue ~queue listener;
    Network.Listener.set_new_connection_handler listener ~handler;
    enter (fun t k ->
        Network.Listener.set_state_changed_handler ~handler:(state_handler t k) listener;
        Network.Listener.start listener
      )
end

class virtual ['a] listening_socket ~backlog:_ net_queue sock = object (self)
  inherit Eio.Net.listening_socket

  method private virtual get_endpoint_addr : Network.Endpoint.t -> Eio.Net.Sockaddr.stream

  val connected = Eio.Semaphore.make 0

  val mutable conn_sock = None;

  val mutable accept_params = None

  method close =
    Network.Listener.cancel sock

  method accept ~sw =
    Eio.Semaphore.acquire connected;
    let (conn, sockaddr) = option_get ~msg:"connection sock" conn_sock in
    Switch.on_release sw (fun () -> Network.Connection.cancel conn);
    (socket conn), sockaddr

  initializer
    let handler (state : Network.Listener.State.t) err =
      match (Network.Error.to_int err, state) with
      | i, _ when i <> 0 -> failwith ("Listener failed with error code: " ^ string_of_int i)
      | _, Ready -> ()
      | _, Failed -> failwith "Network listener failed"
      | _, Invalid -> ()
      | _, Waiting -> ()
      | _, Cancelled -> ()
    in
    let conn_handler conn =
      Network.Connection.retain conn;
      Network.Connection.set_queue ~queue:net_queue conn;
      Network.Connection.start conn;
      let endpoint = Network.Connection.copy_endpoint conn in
      let sockaddr = self#get_endpoint_addr endpoint in
      conn_sock <- Some (conn, sockaddr);
      Eio.Semaphore.release connected
    in
    Network.Listener.set_state_changed_handler ~handler sock;
    Network.Listener.set_new_connection_handler ~handler:conn_handler sock;
    Network.Listener.start sock


end
let listening_socket ~backlog net_queue sock = object
  inherit [[ `TCP ]] listening_socket ~backlog net_queue sock

  method private get_endpoint_addr e =
    match option_get ~msg:"endpoint address" @@ Network.Endpoint.get_address e with
    | Unix.ADDR_UNIX path         -> `Unix path
    | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Ipaddr.of_unix host, port)
end

let net () = object
  inherit Eio.Net.t

  val queue = Dispatch.Queue.create ~typ:Serial ()

  method datagram_socket ~reuse_addr ~reuse_port:_ ~sw:_ addr =
    let open Network in
    let params = Parameters.create_udp () in
    Parameters.set_reuse_local_address params reuse_addr;
    Parameters.set_allow_fast_open params true;
    let listener = match addr with
      | `Udp (_ip, port) ->
        Listener.create_with_port ~port params
      | _ -> Listener.create params
    in
    udp_socket addr listener

  method getnameinfo = Eio_unix.getnameinfo
  method getaddrinfo ~service host =
    let convert : Unix.addr_info -> Eio.Net.Sockaddr.t = fun addr ->
      match addr.ai_addr, addr.ai_protocol with
      | Unix.ADDR_INET (i, p), 6 -> `Tcp (Eio_unix.Ipaddr.of_unix i, p)
      | Unix.ADDR_INET (i, p), 17 -> `Udp (Eio_unix.Ipaddr.of_unix i, p)
      | Unix.ADDR_UNIX p, _ -> `Unix p
      | _ -> failwith "Unknown protcol strategy"
    in
    Unix.getaddrinfo host service []
    |> List.map convert

  method listen ~reuse_addr ~reuse_port:_ ~backlog ~sw:_ = function
    | `Tcp (hostname, port) ->
      let open Network in
      let params = Parameters.create_tcp () in
      let endpoint = Endpoint.create_address Unix.(ADDR_INET (Eio_unix.Ipaddr.to_unix hostname, port)) in
      let _ =
        Parameters.set_reuse_local_address params reuse_addr;
        Parameters.set_local_endpoint ~endpoint params
      in
      let _ = Endpoint.release endpoint in
      let listener = Listener.create_with_port ~port params in
      Listener.set_queue ~queue listener;
      Listener.retain listener;
      listening_socket ~backlog queue listener
    | _ -> assert false

  method connect ~sw:_ = function
    | `Tcp (hostname, port) ->
      let open Network in
      let params = Parameters.create_tcp () in
      let endpoint = Endpoint.create_address Unix.(ADDR_INET (Eio_unix.Ipaddr.to_unix hostname, port)) in
      let _ =
        Parameters.set_reuse_local_address params true
      in
      let connection = Connection.create ~params endpoint in
      let _ = Endpoint.release endpoint in
      Connection.retain connection;
      Connection.set_queue ~queue connection;
      let handler t k (state : Network.Connection.State.t) e =
        match state with
        | Waiting -> ()
        | Ready -> enqueue_thread ~leave_io:true ~msg:"connection" t k (socket connection)
        | Invalid -> enqueue_failed_thread ~leave_io:true ~msg:"connection" t k (Failure (string_of_int @@ Network.Error.to_int e))
        | Preparing -> ()
        | Failed -> enqueue_failed_thread ~leave_io:true ~msg:"connection" t k (Failure (string_of_int @@ Network.Error.to_int e))
        | Cancelled -> enqueue_failed_thread ~leave_io:true ~msg:"connection" t k (Failure (string_of_int @@ Network.Error.to_int e))
      in
      enter (fun t k ->
          PendingIO.enter "conn start" t.pending_io;
          Connection.set_state_changed_handler ~handler:(handler t k) connection;
          Connection.start connection)
    | _ -> assert false
end

type _ Eio.Generic.ty += FD : File.t Eio.Generic.ty

type has_fd = < fd : File.t >
type source = < Eio.Flow.source; Eio.Flow.close; has_fd >
type sink   = < Eio.Flow.sink  ; Eio.Flow.close; has_fd >

let get_fd (t : <has_fd; ..>) = t#fd

let get_fd_opt t = Eio.Generic.probe t FD

let unix_fstat fd =
  let ust = Unix.LargeFile.fstat fd in
  let st_kind : Eio.File.Stat.kind =
    match ust.st_kind with
    | Unix.S_REG  -> `Regular_file
    | Unix.S_DIR  -> `Directory
    | Unix.S_CHR  -> `Character_special
    | Unix.S_BLK  -> `Block_device
    | Unix.S_LNK  -> `Symbolic_link
    | Unix.S_FIFO -> `Fifo
    | Unix.S_SOCK -> `Socket
  in
  Eio.File.Stat.{
    dev     = ust.st_dev   |> Int64.of_int;
    ino     = ust.st_ino   |> Int64.of_int;
    kind    = st_kind;
    perm    = ust.st_perm;
    nlink   = ust.st_nlink |> Int64.of_int;
    uid     = ust.st_uid   |> Int64.of_int;
    gid     = ust.st_gid   |> Int64.of_int;
    rdev    = ust.st_rdev  |> Int64.of_int;
    size    = ust.st_size  |> Optint.Int63.of_int64;
    atime   = ust.st_atime;
    mtime   = ust.st_mtime;
    ctime   = ust.st_ctime;
  }

let flow fd = object (_ : <source; sink; ..>)
  method fd = fd
  method close = File.close fd
  method unix_fd op = File.to_unix op fd

  val io_queue = Dispatch.Queue.create ()

  method probe : type a. a Eio.Generic.ty -> a option = function
    | FD -> Some fd
    | Eio_unix.Private.Unix_file_descr op -> Some (File.to_unix op fd)
    | _ -> None

  method read_into buff =
    let data = Buffer.empty () in
    let res = File.read ~off:0 ~length:(buff.len) io_queue fd data in
    let cs = Cstruct.of_bigarray @@ Dispatch.Data.to_buff ~offset:0 res !data in
    Cstruct.blit cs 0 buff 0 res;
    match res with
    | 0 -> raise End_of_file
    | got -> got

  method read_methods = []

  method pread = failwith "TODO"

  method pwrite = failwith "TODO"

  method stat = unix_fstat (File.to_unix `Peek fd)

  method write _ = failwith "TODO"

  method copy src =
    (* See comment at fast_copy *)
    (* match Eio.Generic.probe src FD with
       | Some src -> File.fast_copy src fd
       | None -> *)
    let chunk = Cstruct.create 4096 in
    try
      while true do
        let got = Eio.Flow.single_read src chunk in
        let chunk = Cstruct.sub chunk 0 got in
        File.write io_queue fd (ref @@ Dispatch.Data.create (Cstruct.to_bigarray chunk))
      done
    with End_of_file -> ()
end

let source fd = (flow fd :> source)
let sink   fd = (flow fd :> sink)

type _ Eio.Generic.ty += Dir_resolve_new : (string -> string) Eio.Generic.ty
let dir_resolve_new x = Eio.Generic.probe x Dir_resolve_new

type Eio.Exn.Backend.t += Gcd_error

let wrap_error = function
  | (Unix.ECONNREFUSED, _, _) -> Eio.Net.err (Connection_failure (Refused Gcd_error))
  | (Unix.ECONNRESET, _, _)| (Unix.EPIPE, _, _) -> Eio.Net.err (Connection_reset Gcd_error)
  | (e, a, b) -> Unix.Unix_error (e, a, b)

let wrap_error_fs e =
  match e with
  | (Unix.EEXIST, _, _) -> Eio.Fs.err (Already_exists Gcd_error)
  | (Unix.ENOENT, _, _) -> Eio.Fs.err (Not_found Gcd_error)
  | e -> wrap_error e

let or_raise_fs = function
  | Ok v -> v
  | Error (Unix.Unix_error (e, a, b)) -> raise (wrap_error_fs (e, a, b))
  | Error exn -> raise exn

type Eio.Exn.Backend.t +=
  | Outside_sandbox of string * string
  | Absolute_path

(* Borrowed mostly from the luv backend *)
class dir ~label (dir_path : string) = object (self)
  inherit Eio.Fs.dir

  val mutable closed = false

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Dir_resolve_new -> Some self#resolve_new
    | _ -> None

  (* Resolve a relative path to an absolute one, with no symlinks.
     @raise Eio.Fs.Permission_denied if it's outside of [dir_path]. *)
  method private resolve path =
    if closed then Fmt.invalid_arg "Attempt to use closed directory %S" dir_path;
    if Filename.is_relative path then (
      let dir_path = File.realpath dir_path |> or_raise_fs in
      let full = File.realpath (Filename.concat dir_path path) |> or_raise_fs in
      let prefix_len = String.length dir_path + 1 in
      if String.length full >= prefix_len && String.sub full 0 prefix_len = dir_path ^ Filename.dir_sep then
        full
      else if full = dir_path then
        full
      else
        raise @@ Eio.Fs.err (Permission_denied (Outside_sandbox (full, dir_path)))
    ) else (
      raise @@ Eio.Fs.err (Permission_denied Absolute_path)
    )

  (* We want to create [path]. Check that the parent is in the sandbox. *)
  method private resolve_new path =
    let dir, leaf = Filename.dirname path, Filename.basename path in
    if leaf = ".." then Fmt.failwith "New path %S ends in '..'!" path
    else
      let dir = self#resolve dir in
      Filename.concat dir leaf

  val clean_up_queue = Dispatch.Queue.create ~typ:Serial ()

  method open_in ~sw path =
    let fd = File.open_ ~sw clean_up_queue (self#resolve path) [ Unix.O_RDONLY ] |> or_raise_fs in
    (flow fd :> <Eio.File.ro; Eio.Flow.close>)

  method open_out ~sw ~append ~create path =
    let mode, flags =
      match create with
      | `Never            -> 0,    []
      | `If_missing  perm -> perm, [ Unix.O_CREAT ]
      | `Or_truncate perm -> perm, [ Unix.O_CREAT; Unix.O_TRUNC ]
      | `Exclusive   perm -> perm, [ Unix.O_CREAT; Unix.O_EXCL ]
    in
    let flags = if append then Unix.O_APPEND :: flags else flags in
    let flags = Unix.O_RDWR :: flags in
    let real_path =
      if create = `Never then self#resolve path
      else self#resolve_new path
    in
    let fd = File.open_ ~sw clean_up_queue real_path flags ~mode |> or_raise_fs in
    (flow fd :> <Eio.File.rw; Eio.Flow.close>)

  method open_dir ~sw path =
    Switch.check sw;
    let label = Filename.basename path in
    let d = new dir ~label (self#resolve path) in
    Switch.on_release sw (fun () -> d#close);
    d

  (* libuv doesn't seem to provide a race-free way to do this. *)
  method mkdir ~perm path =
    let real_path = self#resolve_new path in
    File.mkdir ~mode:perm real_path |> or_raise_fs

  (* libuv doesn't seem to provide a race-free way to do this. *)
  method unlink path =
    let dir_path = Filename.dirname path in
    let leaf = Filename.basename path in
    let real_dir_path = self#resolve dir_path in
    try Unix.unlink (Filename.concat real_dir_path leaf) with (Unix.Unix_error (e, a, b)) -> raise (wrap_error_fs (e, a, b))

  (* libuv doesn't seem to provide a race-free way to do this. *)
  method rmdir path =
    let dir_path = Filename.dirname path in
    let leaf = Filename.basename path in
    let real_dir_path = self#resolve dir_path in
    try Unix.rmdir (Filename.concat real_dir_path leaf) with (Unix.Unix_error (e, a, b)) -> raise (wrap_error_fs (e, a, b))

  method read_dir path =
    let path = self#resolve path in
    Sys.readdir path |> Array.to_list

  method rename old_path new_dir new_path =
    match dir_resolve_new new_dir with
    | None -> invalid_arg "Target is not a luv directory!"
    | Some new_resolve_new ->
      let old_path = self#resolve old_path in
      let new_path = new_resolve_new new_path in
      try Unix.rename old_path new_path with (Unix.Unix_error (e, a, b)) -> raise (wrap_error_fs (e, a, b))

  method close = closed <- true

  method pp f = Fmt.string f (String.escaped label)
end

(* Full access to the filesystem. *)
let fs = object
  inherit dir ~label:"fs" "."

  (* No checks *)
  method! private resolve path = path
end

let cwd = object
  inherit dir  ~label:"cwd" "."
end

external eio_gcd_secure_random : int -> Cstruct.buffer -> unit = "caml_eio_gcd_secure_random"

let secure_random =
  object
    inherit Eio.Flow.source

    method read_into buf =
      let buf = Cstruct.to_bigarray buf in
      let dim = Bigarray.Array1.dim buf in
      eio_gcd_secure_random dim buf;
      dim
  end

let mono_clock = object
  inherit Eio.Time.Mono.t

  val queue = Dispatch.Queue.create ()

  method now = Mtime_clock.now ()

  method sleep_until time =
    let now = Mtime.to_uint64_ns (Mtime_clock.now ()) in
    let time = Mtime.to_uint64_ns time in
    if Int64.unsigned_compare now time >= 0 then Fiber.yield ()
    else (
      let delay_ns = Int64.sub time now |> Int64.max 0L in
      Low_level.sleep_ns ~queue delay_ns
    )
end

let clock = object
  inherit Eio.Time.clock

  val queue = Dispatch.Queue.create ()

  method now = Unix.gettimeofday ()

  method sleep_until due =
    let delay = 1_000_000_000. *. (due -. Unix.gettimeofday ()) |> ceil |> Int64.of_float |> Int64.max 0L in
    Low_level.sleep_ns ~queue delay
end

let domain_mgr ~run_queues ~run_event_loop =
  let run_raw ~pre_spawn fn =
    let domain = ref None in
    enter (fun t k ->
        pre_spawn k;
        domain := Some (Domain.spawn (fun () -> Fun.protect fn ~finally:(fun () ->
          Fiber_context.clear_cancel_fn k.fiber;
          enqueue_thread ~msg:"domain run raw" t k ()))
        )
      );
    Domain.join (Option.get !domain)
  in
  object
    inherit Eio.Domain_manager.t

    method run_raw fn = run_raw ~pre_spawn:ignore fn

    method run fn =
      let cancelled, set_cancelled = Promise.create () in
      let pre_spawn k =
        Fiber_context.set_cancel_fn k.Suspended.fiber (Promise.resolve set_cancelled)
      in
      run_raw ~pre_spawn (fun () ->
          let result = ref None in
          run_event_loop (fun _ -> result := Some (fn ~cancelled));
          Option.get !result
        )

    method submit (type a) (uid : a Eio.Domain_manager.handle) (fn : unit -> a) : a =
      match Hashtbl.find_opt run_queues (Hmap.Key.hide_type uid) with
      | Some (active, cap) when !active >= cap ->
        let p, r = Eio.Promise.create () in
        let _handler, queue = Eio.Domain_manager.lookup_handler_exn uid in
        Queue.push (r, fn) queue;
        Eio.Promise.await p
      | None | Some _ as t ->
          let active = match t with
            | Some (active, _) -> active
            | None ->
              let active = ref 0 in
              (* 3 is an arbitrary number -- we need to be smarter than this when
                  scheduling to ensure no subsystem gets starved. We could potentially
                  know statically ahead of time how many 'subsystems' have registered to
                  submit things to domains, provided they all register at module initialisation
                  time and naively we could do the initial capacities fairly spread amongst
                  these subsystems. *)
              Hashtbl.add run_queues (Hmap.Key.hide_type uid) (active, 3);
              active
          in
          let p, r = Eio.Promise.create () in
          let handler, queue = Eio.Domain_manager.lookup_handler_exn uid in
          Queue.push (r, fn) queue;
          (* Each scheduler is a new Eio loop + the subsystems handler. *)
          let scheduler _ =
            run_event_loop @@ fun _ ->
            Effect.Deep.match_with (fun () ->
              let rec loop () = match Queue.peek_opt queue with
                | Some _ ->
                  let r, task = Queue.pop queue in
                  let v = task () in
                  Eio.Promise.resolve r v;
                  loop ()
                | None ->
                  (* Instead of just leaving immediately we could instead linger
                      a little bit in case some extra work turns up, or block on
                      some condition variable which is broadcast to that will wake
                      us up when the task queue has been modified. *)
                  decr active
              in
                loop ()
            ) () handler
          in
          while not (Eio.Idle_domains.try_spawn ~scheduler) do
            Domain.cpu_relax ();
          done;
          incr active;
          Eio.Promise.await p
  end

type stdenv = <
  stdin  : source;
  stdout : sink;
  stderr : sink;
  net : Eio.Net.t;
  fs : Eio.Fs.dir Eio.Path.t;
  cwd : Eio.Fs.dir Eio.Path.t;
  secure_random : Eio.Flow.source;
  clock : Eio.Time.clock;
  mono_clock : Eio.Time.Mono.t;
  debug : Eio.Debug.t;
  domain_mgr : Eio.Domain_manager.t;
>

let stdenv ~run_queues ~run_event_loop =
  let io_queue = Dispatch.Queue.create ~typ:Serial () in
  let stdin = lazy (source (File.of_gcd_no_hook @@ Dispatch.Io.(create Stream Fd.stdin io_queue))) in
  let stdout = lazy (sink (File.of_gcd_no_hook @@ Dispatch.Io.(create Stream Fd.stdout io_queue))) in
  let stderr = lazy (sink (File.of_gcd_no_hook @@ Dispatch.Io.(create Stream Fd.stderr io_queue))) in
  object (_ : stdenv)
    method stdin  = (Lazy.force stdin)
    method stdout = (Lazy.force stdout)
    method stderr = (Lazy.force stderr)
    method net = net ()
    method fs = (fs :> Eio.Fs.dir), "."
    method cwd = (cwd :> Eio.Fs.dir), "."
    method secure_random = secure_random
    method clock = clock
    method mono_clock = mono_clock
    method debug = Eio.Private.Debug.v
    method domain_mgr = domain_mgr ~run_queues ~run_event_loop
  end

let io_finished t = PendingIO.is_finished t.pending_io

(* This is a mix of the uring-based linux scheduler and the
   luv-based scheduler. *)

let rec schedule st : [ `Exit_scheduler ] =
  match Lf_queue.pop st.run_q with
  | Some (Thread (_msg, f)) ->
    f ()
  | Some IO ->
    if not (Lf_queue.is_empty st.run_q) then (
      Lf_queue.push st.run_q IO;
      schedule st
    ) else if io_finished st then (
      `Exit_scheduler
    ) else (
      Mutex.lock st.wakeup_lock;
      Atomic.set st.need_wakeup true;
      if Lf_queue.is_empty st.run_q then (
        Mutex.unlock st.wakeup_lock;
        let v = Dispatch.(Semaphore.wait st.wakeup (Time.forever ())) in
        Atomic.set st.need_wakeup false;
        Lf_queue.push st.run_q IO;
        schedule st
      ) else (
        Atomic.set st.need_wakeup false;
        Mutex.unlock st.wakeup_lock;
        Lf_queue.push st.run_q IO;
        schedule st
      )
    )
  | None -> assert false

let enqueue_at_head t k v =
  Lf_queue.push_head t.run_q (Thread ("fork", fun () -> Suspended.continue k v));
  if Atomic.get t.need_wakeup then wakeup t

let run_queues =
  Eio.Idle_domains.prepare 4;
  Hashtbl.create 16

let name =
  let i = ref 0 in
  fun () -> incr i; string_of_int !i

let rec run : type a. (_ -> a) -> a = fun main ->
  let open Eio.Private in
  let run_q = Lf_queue.create () in
  Lf_queue.push run_q IO;
  let pending_io = PendingIO.create () in
  let async = Dispatch.Queue.create () in
  let wakeup = Dispatch.Semaphore.create 0 in
  let need_wakeup = Atomic.make false in
  let wakeup_lock = Mutex.create () in
  let st = { async; pending_io; run_q; need_wakeup; wakeup; wakeup_lock; name = name (); } in
  let rec fork ~new_fiber:fiber fn =
    Ctf.note_switch (Fiber_context.tid fiber);
    let open Effect.Deep in
    match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; schedule st );
        exnc = (fun e -> Fiber_context.destroy fiber; raise e);
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Eio.Private.Effects.Fork (new_fiber, f) ->
            Some (fun (k : (a, _) continuation) ->
                let k = { Suspended.k; fiber } in
                enqueue_at_head st k ();
                fork ~new_fiber f
              )
          | Eio.Private.Effects.Get_context -> Some (fun k -> continue k fiber)
          | Enter fn -> Some (fun k ->
              match Fiber_context.get_error fiber with
              | Some e -> discontinue k e
              | None -> (
                  fn st { Suspended.k; fiber };
                  schedule st
                )
            )
          | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k mono_clock)
          | Eio.Private.Effects.Suspend fn ->
            Some (fun k ->
                let k = { Suspended.k; fiber } in
                fn fiber (function
                    | Ok v -> enqueue_thread ~msg:"suspend" st k v
                    | Error ex -> enqueue_failed_thread ~msg:"suspend" st k ex
                  );
                schedule st
              )
          | Eio_unix.Private.Pipe sw -> Some (fun k ->
              let r, w = Unix.pipe ~cloexec:true () in
              (* See issue #319, PR #327 *)
              Unix.set_nonblock r;
              Unix.set_nonblock w;
              let r = Dispatch.Io.(create Stream Fd.(of_unix r) async) in
              let w = Dispatch.Io.(create Stream Fd.(of_unix w) async) in
              let r = (flow (File.of_gcd ~sw r) :> <Eio.Flow.source; Eio.Flow.close; Eio_unix.unix_fd>) in
              let w = (flow (File.of_gcd ~sw w) :> <Eio.Flow.sink; Eio.Flow.close; Eio_unix.unix_fd>) in
              continue k (r, w)
            )
          | Eio_unix.Private.Socketpair _ -> Some (fun k ->
              discontinue k (Failure "Socketpair not supported by GCD backend yet")
            )
          | _ -> None
      }
  in
  let result = ref None in
  let exit_group = Dispatch.Group.create () in
  let `Exit_scheduler =
    let new_fiber = Fiber_context.make_root () in
    Dispatch.Group.enter exit_group;
    fork ~new_fiber (fun () ->
        Switch.run_protected (fun sw ->
            Switch.on_release sw (fun () -> ());
            let v = main (stdenv ~run_queues ~run_event_loop:run) in
            result := Some v;
            Dispatch.Group.leave exit_group
          )
      )
  in
  let _state = Dispatch.Group.wait exit_group (Dispatch.Time.forever ()) in
  Lf_queue.close st.run_q;
  option_get ~msg:"end result" !result
