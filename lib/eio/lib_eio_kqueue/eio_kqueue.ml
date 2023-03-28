(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 * Copyright (C) 2023 Patrick Ferris
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

open Eio

module Ctf = Eio.Private.Ctf
module Lf_queue = Eio_utils.Lf_queue
module Suspended = Eio_utils.Suspended
module Fiber_context = Eio.Private.Fiber_context

type Eio.Exn.Backend.t +=
  | Kqueue_unix of Unix.error * string * string
  | Outside_sandbox of string * string
  | Absolute_path

let unclassified_error e = Eio.Exn.create (Eio.Exn.X e)

let () =
  Eio.Exn.Backend.register_pp (fun f -> function
      | Outside_sandbox (path, dir) -> Fmt.pf f "Outside_sandbox (%S, %S)" path dir; true
      | Absolute_path -> Fmt.pf f "Absolute_path"; true
      | _ -> false
    )

let wrap_error_fs code name arg =
  let e = Eio_unix.Unix_error (code, name, arg) in
  match code with
  | Unix.EEXIST -> Eio.Fs.err (Already_exists e)
  | Unix.ENOENT -> Eio.Fs.err (Not_found e)
  | Unix.EXDEV -> Eio.Fs.err (Permission_denied e)
  | _ -> Unix.Unix_error (code, name, arg)

let wrap_unix_fn unix_fn =
  try unix_fn () with Unix.Unix_error (code, name, arg) -> raise @@ wrap_error_fs code name arg

type _ Effect.t += Close : Unix.file_descr -> int Effect.t

type runnable =
  | IO : runnable
  | Thread : 'a Suspended .t * 'a -> runnable
  | Failed_thread : 'a Suspended.t * exn -> runnable

module FD = struct
  type t = {
    seekable : bool;
    close_unix : bool;                          (* Whether closing this also closes the underlying FD. *)
    mutable release_hook : Eio.Switch.hook;     (* Use this on close to remove switch's [on_release] hook. *)
    mutable fd : [`Open of Unix.file_descr | `Closed]
  }

  let get_exn op = function
    | { fd = `Open fd; _ } -> fd
    | { fd = `Closed ; _ } -> invalid_arg (op ^ ": file descriptor used after calling close!")

  let get op = function
    | { fd = `Open fd; _ } -> Ok fd
    | { fd = `Closed ; _ } -> Error (Invalid_argument (op ^ ": file descriptor used after calling close!"))

  let is_open = function
    | { fd = `Open _; _ } -> true
    | { fd = `Closed; _ } -> false

  let close t =
    Ctf.log "close";
    let fd = get_exn "close" t in
    t.fd <- `Closed;
    Eio.Switch.remove_hook t.release_hook;
    if t.close_unix then (
      let res = Effect.perform (Close fd) in
      if res < 0 then
        failwith "CLOSE!"
    )

  let ensure_closed t =
    if is_open t then close t

  let is_seekable fd =
    match Unix.lseek fd 0 Unix.SEEK_CUR with
    | (_ : int) -> true
    | exception Unix.Unix_error(Unix.ESPIPE, "lseek", "") -> false

  let to_unix op t =
    let fd = get_exn "to_unix" t in
    match op with
    | `Peek -> fd
    | `Take ->
      t.fd <- `Closed;
      Eio.Switch.remove_hook t.release_hook;
      fd

  let of_unix_no_hook ~seekable ~close_unix fd =
    { seekable; close_unix; fd = `Open fd; release_hook = Eio.Switch.null_hook }

  let of_unix ~sw ~seekable ~close_unix fd =
    let t = of_unix_no_hook ~seekable ~close_unix fd in
    t.release_hook <- Switch.on_release_cancellable sw (fun () -> ensure_closed t);
    t

  let fstat t =
    try
      let ust = Unix.LargeFile.fstat (get_exn "fstat" t) in
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
    with Unix.Unix_error (code, name, arg) -> raise @@ wrap_error_fs code name arg
end

(* We follow the Eio_linux way of doing timeouts as it is easier
   to avoid the busy yielding issue that way than using actual
   events. *)
type _ Effect.t += Sleep_until : Mtime.t -> unit Effect.t
let sleep_until d = Effect.perform (Sleep_until d)

type io_job =
  | Read : Cstruct.t * (int * bool) Suspended.t -> io_job
  | Writev : Cstruct.t list * unit Suspended.t -> io_job
  | Recv_msg : Cstruct.t * (Unix.sockaddr * int) Suspended.t -> io_job
  | Send_msg : Unix.sockaddr * Cstruct.t * unit Suspended.t -> io_job
  | Accept : Switch.t * (FD.t * Unix.sockaddr) Suspended.t -> io_job
  | Connect : unit Suspended.t -> io_job
  | Clock : unit Suspended.t -> io_job

type _ Effect.t += Cancel : (io_job Heap.entry * exn * Kq.Events.t) -> unit Effect.t

let cancel job exn event = Effect.perform (Cancel (job, exn, event))

type t = {
  kqueue : io_job Kq.t;
  run_q : runnable Lf_queue.t;
  pending_io : int Atomic.t;
  read_pipe : FD.t;
  write_pipe : FD.t;
  need_wakeup : bool Atomic.t;
}

let wake_buffer = Bytes.create 8

let wakeup t =
  Atomic.set t.need_wakeup false; (* [t] will check [run_q] after getting the event below *)
  match t.write_pipe.fd with
  | `Closed -> ()       (* Domain has shut down (presumably after handling the event) *)
  | `Open fd ->
    let sent = Unix.single_write fd wake_buffer 0 8 in
    assert (sent = 8)

type _ Effect.t += Enter : (t -> 'a Suspended.t -> unit) -> 'a Effect.t

let enter f = Effect.perform (Enter f)

let enqueue_thread st k v =
  Lf_queue.push st.run_q (Thread (k, v));
  if Atomic.get st.need_wakeup then wakeup st

let enqueue_failed_thread st k exn =
  Lf_queue.push st.run_q (Failed_thread (k, exn));
  if Atomic.get st.need_wakeup then wakeup st

let enqueue_at_head st k v =
  Lf_queue.push_head st.run_q (Thread (k, v));
  if Atomic.get st.need_wakeup then wakeup st

let discontinue_io st exn = function
  | Read (_, k) -> enqueue_failed_thread st k exn
  | Writev (_, k) -> enqueue_failed_thread st k exn
  | Recv_msg (_, k) -> enqueue_failed_thread st k exn
  | Send_msg (_, _, k) -> enqueue_failed_thread st k exn
  | Accept (_, k) -> enqueue_failed_thread st k exn
  | Connect k -> enqueue_failed_thread st k exn
  | Clock k -> enqueue_failed_thread st k exn

let with_cancel_hook ~action st fn =
  match Fiber_context.get_error action.Suspended.fiber with
  | Some ex -> enqueue_failed_thread st action ex; false
  | None ->
    match fn () with
    | None -> true
    | Some (event, job) ->
      Fiber_context.set_cancel_fn action.fiber (fun exn -> cancel job exn event);
      false

module Low_level = struct
  let ignore_nonblocking_catch_err fn =
    try fn () with
    | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
    | Unix.Unix_error (Unix.EAGAIN, _, _)
    | Unix.Unix_error (Unix.EINPROGRESS, _, _) -> ()
    | Unix.Unix_error (Unix.ENOENT, a, b) -> raise @@ Eio.Fs.err (Not_found (Kqueue_unix (Unix.ENOENT, a, b)))
    | Unix.Unix_error (Unix.ECONNRESET, a, b) -> raise @@ Eio.Net.err (Connection_reset (Kqueue_unix (Unix.ECONNRESET, a, b)))
    | Unix.Unix_error (Unix.EPIPE, a, b) -> raise @@ Eio.Net.err (Connection_reset (Kqueue_unix (Unix.EPIPE, a, b)))
    | exn -> raise exn

  let sleep_ms ~ms =
    enter @@ fun st action ->
    Atomic.incr st.pending_io;
    let retry = with_cancel_hook ~action st (fun () ->
        let flags = Kqueue.Flag.(add + oneshot) in
        let filter = Kqueue.Filter.timer in
        (* No ident is passed, Kq will use the unique heap pointer
           which will allow us to cancel the event should we need to. *)
        Kq.submit ~flags ~filter ~data:ms st.kqueue (Clock action)
      ) in
    if retry then Eio.traceln "TODO: retry"

  let open_ ~sw path ~access:_ ~flags:file_flags ~perms =
    let fd = wrap_unix_fn (fun () -> Unix.openfile path (Unix.O_NONBLOCK :: file_flags) perms) in
    FD.of_unix ~sw ~seekable:true ~close_unix:true fd

  let read fd buff : int * bool =
    let fd = FD.get_exn "read" fd in
    enter @@ fun st action ->
    Atomic.incr st.pending_io;
    let retry = with_cancel_hook ~action st (fun () ->
        let flags = Kqueue.Flag.(add + oneshot) in
        let filter = Kqueue.Filter.read in
        Kq.submit ~flags ~filter ~ident:(Kq.Ident.of_fd fd) st.kqueue (Read (buff, action))
      ) in
    if retry then Eio.traceln "TODO: retry"

  let write fd buff : unit =
    let fd = FD.get_exn "write" fd in
    enter @@ fun st action ->
    Atomic.incr st.pending_io;
    let retry = with_cancel_hook ~action st (fun () ->
        let flags = Kqueue.Flag.(add + oneshot) in
        let filter = Kqueue.Filter.write in
        Kq.submit ~filter ~flags ~ident:(Kq.Ident.of_fd fd) st.kqueue (Writev (buff, action))
      ) in
    if retry then Eio.traceln "TODO: retry"

  let accept ~sw fd =
    let fd = FD.get_exn "accept" fd in
    enter @@ fun st action ->
    Atomic.incr st.pending_io;
    let retry = with_cancel_hook ~action st (fun () ->
        let flags = Kqueue.Flag.(add + oneshot) in
        (* See EVFILT_READ for kqueue(2), fd here is being listened on. *)
        let filter = Kqueue.Filter.read in
        Kq.submit ~filter ~flags ~ident:(Kq.Ident.of_fd fd) st.kqueue (Accept (sw, action))
      ) in
    if retry then Eio.traceln "TODO: retry"

  let send_msg fd ~dst buff =
    let fd = FD.get_exn "send_msg" fd in
    enter @@ fun st action ->
    Atomic.incr st.pending_io;
    let retry = with_cancel_hook ~action st (fun () ->
        let flags = Kqueue.Flag.(add + oneshot) in
        let filter = Kqueue.Filter.write in
        Kq.submit ~flags ~filter ~ident:(Kq.Ident.of_fd fd) st.kqueue (Send_msg (dst, buff, action))
      ) in
    if retry then Eio.traceln "TODO: retry"

  let recv_msg fd buff =
    let fd = FD.get_exn "recv_msg" fd in
    enter @@ fun st action ->
    Atomic.incr st.pending_io;
    let retry = with_cancel_hook ~action st (fun () ->
        let flags = Kqueue.Flag.(add + oneshot) in
        let filter = Kqueue.Filter.read in
        Kq.submit ~flags ~filter ~ident:(Kq.Ident.of_fd fd) st.kqueue (Recv_msg (buff, action))
      ) in
    if retry then Eio.traceln "TODO: retry"

  (* Waiting for writable in order to wait for connect. *)
  let connect fd addr =
    let fd = FD.get_exn "connect" fd in
    Unix.set_nonblock fd;
    ignore_nonblocking_catch_err (fun () -> Unix.connect fd addr);
    enter @@ fun st action ->
    Atomic.incr st.pending_io;
    let retry = with_cancel_hook ~action st (fun () ->
        let flags = Kqueue.Flag.(add + oneshot) in
        let filter = Kqueue.Filter.write in
        Kq.submit ~filter ~flags ~ident:(Kq.Ident.of_fd fd) st.kqueue (Connect action)
      ) in
    if retry then Eio.traceln "TODO: retry"

  external eio_arc4random : Cstruct.buffer -> int -> int -> int = "caml_eio_arc4random"

  let arc4random { Cstruct.buffer; off; len } =
    let got = eio_arc4random buffer off len in
    assert (len = got)
end

let fallback_copy src dst =
  let fallback () =
    (* No chunks available. Use regular memory instead. *)
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Eio.Flow.single_read src buf in
        Low_level.write dst [ Cstruct.sub buf 0 got ]
      done
    with End_of_file -> ()
  in
  fallback ()

let flow fd =
  object (_ : <Flow.source; Flow.sink; ..>)
    method fd = fd
    method close = FD.close fd

    method stat = FD.fstat fd

    method probe : type a. a Eio.Generic.ty -> a option = function
      (* | FD -> Some fd *)
      | Eio_unix.Private.Unix_file_descr op -> Some (FD.to_unix op fd)
      | _ -> None

    val mutable eof = false

    method read_into buf =
      if eof then raise End_of_file;
      let got, end_of_file = Low_level.read fd buf in
      if got = 0 then raise End_of_file
      else if got > 0 && end_of_file then (eof <- end_of_file; got)
      else got

    method pread ~file_offset:_ _bufs = failwith "TODO PREAD"

    method pwrite ~file_offset:_ _bufs = failwith "TODO PWRITE"

    method read_methods = []

    method write bufs = Low_level.write fd bufs

    method copy src =
      (* match get_fd_opt src with
         | Some src -> fast_copy_try_splice src fd
         | None -> *)
      let rec aux = function
        | Eio.Flow.Read_source_buffer _rsb :: xs ->
          (* TODO: faster copy here. *)
          aux xs
        | _ :: xs -> aux xs
        | [] -> fallback_copy src fd
      in
      aux (Eio.Flow.read_methods src)

    method shutdown cmd =
      Unix.shutdown (FD.get_exn "shutdown" fd) @@ match cmd with
      | `Receive -> Unix.SHUTDOWN_RECEIVE
      | `Send -> Unix.SHUTDOWN_SEND
      | `All -> Unix.SHUTDOWN_ALL

    method unix_fd op = FD.to_unix op fd
  end

let pp_sockaddr ppf = function
  | Unix.ADDR_INET (ip, port) -> Fmt.pf ppf "%a:%i" Net.Ipaddr.pp (Eio_unix.Ipaddr.of_unix ip) port
  | _ -> ()

(* Seems EOF can either be signalled in the event itself, or
   the data associated with the event is how many bytes are left
   to read. *)
let check_read_eof ~got event =
  Kqueue.Flag.(intersect eof (Kq.Events.get_flags event)) ||
  Kq.Events.get_data event = got

let rec schedule st = match Lf_queue.pop st.run_q with
  | Some (Thread (t, v)) ->
    Fiber_context.clear_cancel_fn t.fiber;
    Suspended.continue t v
  | Some (Failed_thread (t, exn)) ->
    Fiber_context.clear_cancel_fn t.fiber;
    Suspended.discontinue t exn
  | Some IO -> (
      (* First we do a quick check for some events, this prevents
         starving the event loop with a busy yielder! *)
      let evs = Kq.wait ~timeout:Kqueue.Timeout.immediate ~max_events:16 st.kqueue in
      match evs with
      | _ :: _ as xs ->
        Lf_queue.push st.run_q IO;
        List.iter (complete_io st) xs;
        schedule st
      | [] ->
        if not (Lf_queue.is_empty st.run_q) then begin
          Lf_queue.push st.run_q IO;                   (* Re-inject IO job in the run queue *)
          schedule st
        end else
        if Atomic.get st.pending_io = 0 then begin
          Lf_queue.close st.run_q;
          `Exit_scheduler
        end else begin
          Atomic.set st.need_wakeup true;
          if Lf_queue.is_empty st.run_q then begin
            let readies = Kq.wait ~max_events:16 st.kqueue in
            Lf_queue.push st.run_q IO;
            List.iter (complete_io st) readies;
            Atomic.set st.need_wakeup false;
            schedule st
          end else begin
            Lf_queue.push st.run_q IO;
            Atomic.set st.need_wakeup false;
            schedule st
          end
        end
    )
  | None -> assert false

and check_for_error : 'a. t -> 'a Suspended.t -> Kq.Events.t -> unit = fun st k event ->
  let flags = Kq.Events.get_flags event in
  if Kqueue.Flag.(intersect flags Kqueue.Flag.error) then begin
    enqueue_failed_thread st k (Failure "READ");
    Atomic.decr st.pending_io
  end else ()

and complete_io st ((event, ready) : io_job Kq.ready) = match ready with
  | Read (buff, k) ->
    Fiber_context.clear_cancel_fn k.fiber;
    check_for_error st k event;
    let fd = Kqueue.Util.file_descr_of_int (Kq.Events.get_ident event) in
    let got = Unix_cstruct.read fd buff in
    if check_read_eof ~got event then begin
      enqueue_thread st k (got, true);
      Atomic.decr st.pending_io
    end else begin
      enqueue_thread st k (got, false);
      Atomic.decr st.pending_io
    end
  | Writev (buffs, k) ->
    Fiber_context.clear_cancel_fn k.fiber;
    check_for_error st k event;
    (* The filter will set EV_EOF when the reader disconnects *)
    if Kqueue.Flag.(intersect eof (Kq.Events.get_flags event)) then begin
      enqueue_failed_thread st k (Eio.Net.err (Connection_reset (Kqueue_unix (Unix.ECONNRESET, "kqueue_writev", ""))));
      Atomic.decr st.pending_io
    end else begin
      let fd = Kqueue.Util.file_descr_of_int (Kq.Events.get_ident event) in
      Unix_cstruct.writev fd buffs;
      enqueue_thread st k ();
      Atomic.decr st.pending_io
    end
  | Send_msg (sockaddr, buffs, k) ->
    Fiber_context.clear_cancel_fn k.fiber;
    check_for_error st k event;
    (* The filter will set EV_EOF when the reader disconnects *)
    if Kqueue.Flag.(intersect eof (Kq.Events.get_flags event)) then begin
      enqueue_failed_thread st k (Eio.Net.err (Connection_reset (Kqueue_unix (Unix.ECONNRESET, "kqueue_writev", ""))));
      Atomic.decr st.pending_io
    end else begin
      let fd = Kqueue.Util.file_descr_of_int (Kq.Events.get_ident event) in
      ignore (Unix_cstruct.sendto fd buffs [] sockaddr);
      enqueue_thread st k ();
      Atomic.decr st.pending_io
    end
  | Recv_msg (buffs, k) ->
    Fiber_context.clear_cancel_fn k.fiber;
    check_for_error st k event;
    let fd = Kqueue.Util.file_descr_of_int (Kq.Events.get_ident event) in
    let read, sockaddr = Unix_cstruct.recvfrom fd buffs [] in
    enqueue_thread st k (sockaddr, read);
    Atomic.decr st.pending_io
  | Accept (sw, k) ->
    Fiber_context.clear_cancel_fn k.fiber;
    check_for_error st k event;
    let fd = Kqueue.Util.file_descr_of_int (Kq.Events.get_ident event) in
    let conn, addr = Unix.accept ~cloexec:true fd in
    enqueue_thread st k (FD.of_unix ~sw ~seekable:false ~close_unix:true conn, addr);
    Atomic.decr st.pending_io
  | Connect k -> (
      Fiber_context.clear_cancel_fn k.fiber;
      check_for_error st k event;
      let fd = Kqueue.Util.file_descr_of_int (Kq.Events.get_ident event) in
      try
        (* Sanity check that we truly are connected! *)
        let _sockaddr = Unix.getpeername fd in
        enqueue_thread st k ();
        Atomic.decr st.pending_io
      with Unix.Unix_error (Unix.EINVAL, a, b) -> (
          enqueue_failed_thread st k (Eio.Net.err (Connection_failure (Refused (Kqueue_unix (Unix.EINVAL, a, b)))));
          Atomic.decr st.pending_io
        )
    )
  | Clock k -> (
      Fiber_context.clear_cancel_fn k.fiber;
      check_for_error st k event;
      enqueue_thread st k ();
      Atomic.decr st.pending_io
    )

let listening_socket fd = object
  inherit Eio.Net.listening_socket

  method! probe : type a. a Eio.Generic.ty -> a option = function
    | Eio_unix.Private.Unix_file_descr op -> Some (FD.to_unix op fd)
    | _ -> None

  method close = FD.close fd

  method accept ~sw =
    Switch.check sw;
    let client, client_addr = Low_level.accept ~sw fd in
    let client_addr = match client_addr with
      | Unix.ADDR_UNIX path         -> `Unix path
      | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Ipaddr.of_unix host, port)
    in
    let flow = (flow client :> <Eio.Flow.two_way; Eio.Flow.close>) in
    flow, client_addr
end

let socket_domain_of = function
  | `Unix _ -> Unix.PF_UNIX
  | `UdpV4 -> Unix.PF_INET
  | `UdpV6 -> Unix.PF_INET6
  | `Udp (host, _)
  | `Tcp (host, _) ->
    Eio.Net.Ipaddr.fold host
      ~v4:(fun _ -> Unix.PF_INET)
      ~v6:(fun _ -> Unix.PF_INET6)

let udp_socket sock = object
  inherit Eio.Net.datagram_socket

  method close = FD.close sock

  method send sockaddr buf =
    let addr = match sockaddr with
      | `Udp (host, port) ->
        let host = Eio_unix.Ipaddr.to_unix host in
        Unix.ADDR_INET (host, port)
    in
    Low_level.send_msg sock ~dst:addr buf

  method recv buf =
    let addr, recv = Low_level.recv_msg sock buf in
    match addr with
    | Unix.ADDR_INET (inet, port) ->
      `Udp (Eio_unix.Ipaddr.of_unix inet, port), recv
    | Unix.ADDR_UNIX _ ->
      raise (Failure "Expected INET UDP socket address but got Unix domain socket address.")
end

let net = object
  inherit Eio.Net.t

  method listen ~reuse_addr ~reuse_port  ~backlog ~sw listen_addr =
    let socket_type, addr =
      match listen_addr with
      | `Unix path         ->
        if reuse_addr then (
          match Unix.lstat path with
          | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
          | _ -> ()
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
          | exception exn -> raise exn
        );
        Unix.SOCK_STREAM, Unix.ADDR_UNIX path
      | `Tcp (host, port)  ->
        let host = Eio_unix.Ipaddr.to_unix host in
        Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
    in
    let sock_unix = Unix.socket (socket_domain_of listen_addr) socket_type 0 in
    (* For Unix domain sockets, remove the path when done (except for abstract sockets). *)
    begin match listen_addr with
      | `Unix path ->
        if String.length path > 0 && path.[0] <> Char.chr 0 then
          Switch.on_release sw (fun () -> Unix.unlink path)
      | `Tcp _ -> ()
    end;
    if reuse_addr then
      Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
    if reuse_port then
      Unix.setsockopt sock_unix Unix.SO_REUSEPORT true;
    let sock = FD.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    Unix.bind sock_unix addr;
    Unix.listen sock_unix backlog;
    listening_socket sock

  method connect ~sw connect_addr =
    let socket_type, addr =
      match connect_addr with
      | `Unix path         -> Unix.SOCK_STREAM, Unix.ADDR_UNIX path
      | `Tcp (host, port)  ->
        let host = Eio_unix.Ipaddr.to_unix host in
        Unix.SOCK_STREAM, Unix.ADDR_INET (host, port)
    in
    let sock_unix = Unix.socket (socket_domain_of connect_addr) socket_type 0 in
    let sock = FD.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    Low_level.connect sock addr;
    (flow sock :> <Eio.Flow.two_way; Eio.Flow.close>)

  method datagram_socket ~reuse_addr ~reuse_port ~sw saddr =
    let sock_unix = Unix.socket ~cloexec:true (socket_domain_of saddr) Unix.SOCK_DGRAM 0 in
    let sock = FD.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    begin match saddr with
      | `Udp (host, port) ->
        let host = Eio_unix.Ipaddr.to_unix host in
        let addr = Unix.ADDR_INET (host, port) in
        if reuse_addr then
          Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
        if reuse_port then
          Unix.setsockopt sock_unix Unix.SO_REUSEPORT true;
        Unix.bind sock_unix addr
      | `UdpV4 | `UdpV6 -> ()
    end;
    udp_socket sock

  method getaddrinfo ~service node =
    let to_eio_sockaddr_t {Unix.ai_family; ai_addr; ai_socktype; ai_protocol; _ } =
      match ai_family, ai_socktype, ai_addr with
      | (Unix.PF_INET | PF_INET6),
        (Unix.SOCK_STREAM | SOCK_DGRAM),
        Unix.ADDR_INET (inet_addr,port) -> (
          match ai_protocol with
          | 6 -> Some (`Tcp (Eio_unix.Ipaddr.of_unix inet_addr, port))
          | 17 -> Some (`Udp (Eio_unix.Ipaddr.of_unix inet_addr, port))
          | _ -> None)
      | _ -> None
    in
    Unix.getaddrinfo node service []
    |> List.filter_map to_eio_sockaddr_t

  method getnameinfo sockaddr =
    let sockaddr, options =
      match sockaddr with
      | `Unix s -> (Unix.ADDR_UNIX s, [])
      | `Tcp (addr, port) -> (Unix.ADDR_INET (Eio_unix.Ipaddr.to_unix addr, port), [])
      | `Udp (addr, port) -> (Unix.ADDR_INET (Eio_unix.Ipaddr.to_unix addr, port), [Unix.NI_DGRAM])
    in
    let Unix.{ni_hostname; ni_service} = Unix.getnameinfo sockaddr options in
    (ni_hostname, ni_service)
end

type _ Eio.Generic.ty += Dir_resolve_new : (string -> string) Eio.Generic.ty
let dir_resolve_new x = Eio.Generic.probe x Dir_resolve_new

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
      let dir_path = wrap_unix_fn (fun () -> Unix.realpath dir_path) in
      let full = wrap_unix_fn (fun () -> Unix.realpath (Filename.concat dir_path path)) in
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

  method open_in ~sw path =
    let fd = Low_level.open_ ~sw (self#resolve path)
        ~access:`R
        ~flags:[ Unix.O_CLOEXEC; Unix.O_RDONLY ]
        ~perms:0
    in
    (flow fd :> <Eio.File.ro; Eio.Flow.close>)

  method open_out ~sw ~append ~create path =
    let perms, flags =
      match create with
      | `Never            -> 0,    []
      | `If_missing  perm -> perm, [ Unix.O_CREAT ]
      | `Or_truncate perm -> perm, [ Unix.O_CREAT; Unix.O_TRUNC ]
      | `Exclusive   perm -> perm, [ Unix.O_CREAT; Unix.O_EXCL ]
    in
    let flags = if append then Unix.O_APPEND :: flags else flags in
    let real_path =
      if create = `Never then self#resolve path
      else self#resolve_new path
    in
    let fd = Low_level.open_ ~sw real_path
        ~access:`W
        ~flags:(Unix.O_CLOEXEC :: Unix.O_RDWR :: flags)
        ~perms
    in
    (flow fd :> <Eio.File.rw; Eio.Flow.close>)

  method open_dir ~sw path =
    Switch.check sw;
    let label = Filename.basename path in
    let d = new dir ~label (self#resolve path) in
    Switch.on_release sw (fun () -> d#close);
    d

  method mkdir ~perm path =
    let real_path = self#resolve_new path in
    wrap_unix_fn (fun () -> Unix.mkdir real_path perm)

  method read_dir path =
    let real_path = self#resolve path in
    wrap_unix_fn (fun () -> Sys.readdir real_path) |> Array.to_list

  method close = closed <- true

  method unlink path =
    let dir_path = Filename.dirname path in
    let leaf = Filename.basename path in
    let real_dir_path = self#resolve dir_path in
    wrap_unix_fn (fun () -> Unix.unlink (Filename.concat real_dir_path leaf))

  method rmdir path =
    let dir_path = Filename.dirname path in
    let leaf = Filename.basename path in
    let real_dir_path = self#resolve dir_path in
    wrap_unix_fn (fun () -> Unix.rmdir (Filename.concat real_dir_path leaf))

  method rename old_path new_dir new_path =
    match dir_resolve_new new_dir with
    | None -> invalid_arg "Target is not a luv directory!"
    | Some new_resolve_new ->
      let old_path = self#resolve old_path in
      let new_path = new_resolve_new new_path in
      wrap_unix_fn (fun () -> Unix.rename old_path new_path)

  method pp f = Fmt.string f (String.escaped label)
end

type stdenv = <
  stdin  : Flow.source;
  stdout : Flow.sink;
  stderr : Flow.sink;
  net : Eio.Net.t;
  domain_mgr : Eio.Domain_manager.t;
  clock : Eio.Time.clock;
  mono_clock : Eio.Time.Mono.t;
  fs : Eio.Fs.dir Eio.Path.t;
  cwd : Eio.Fs.dir Eio.Path.t;
  secure_random : Eio.Flow.source;
  debug : Eio.Debug.t;
>

let monitor_event_fd t =
  let buf = Cstruct.create 8 in
  while true do
    let got, _ = Low_level.read t.read_pipe buf in
    assert (got = 8);
    (* We just go back to sleep now, but this will cause the scheduler to look
       at the run queue again and notice any new items. *)
  done;
  assert false

let domain_mgr ~run_queues ~run_event_loop = object
  inherit Eio.Domain_manager.t

  method run_raw fn =
    let domain = ref None in
    enter (fun t k ->
        domain := Some (Domain.spawn (fun () -> Fun.protect fn ~finally:(fun () -> enqueue_thread t k ())))
      );
    Domain.join (Option.get !domain)

  method run fn =
    let domain = ref None in
    enter (fun t k ->
        let cancelled, set_cancelled = Promise.create () in
        Fiber_context.set_cancel_fn k.fiber (Promise.resolve set_cancelled);
        domain := Some (Domain.spawn (fun () ->
            Fun.protect
              (fun () ->
                 let result = ref None in
                 run_event_loop (fun _ -> result := Some (fn ~cancelled));
                 Option.get !result
              )
              ~finally:(fun () -> enqueue_thread t k ())))
      );
    Domain.join (Option.get !domain)


  (* When submitting tasks to run on different domains we need a few things:
      - We need the tasks effect handler to install in an idle domain
      - We need the handle's task queue to push new items onto
      - We need to know how many of the "idle domains" are currently active
        for our 'subsystem'.
         - If we have reach capacity then we can push items to the work queue
           and one of the active domains will pick it up.
         - If we haven't reached capacity then we can spin up a new idle domain
           for our subsystem.

      ... all of this modulo making it domain safe ^^'
  *)
  method submit (type a) (uid : a Eio.Domain_manager.handle) (fn : unit -> a) : a =
    match Hashtbl.find_opt run_queues (Hmap.Key.hide_type uid) with
    | Some (active, cap) when !active >= cap ->
      Eio.traceln "At capacity, pushing items to work queue.";
      let p, r = Eio.Promise.create () in
      let _handler, queue = Eio.Domain_manager.lookup_handler_exn uid in
      Queue.push (r, fn) queue;
      Eio.Promise.await_exn p
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
                let v = 
                  try Ok (task ()) with exn -> Error exn 
                in
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
        Eio.Promise.await_exn p


end

let fs = object
  inherit dir ~label:"fs" "."

  (* No checks *)
  method! private resolve path = path
end

let cwd = object
  inherit dir  ~label:"cwd" "."
end

let clock = object
  inherit Eio.Time.clock

  method now = Unix.gettimeofday ()

  method sleep_until due =
    let ms = 1000. *. (due -. Unix.gettimeofday ()) |> ceil |> truncate |> max 0 in
    Low_level.sleep_ms ~ms
end

let mono_clock = object
  inherit Eio.Time.Mono.t

  method now = Mtime_clock.now ()

  method sleep_until time =
    let now = Mtime.to_uint64_ns (Mtime_clock.now ()) in
    let time = Mtime.to_uint64_ns time in
    if Int64.unsigned_compare now time >= 0 then Fiber.yield ()
    else (
      let delay_ns = Int64.sub time now |> Int64.to_float in
      let delay_ms = delay_ns /. 1e6 |> ceil |> truncate |> max 0 in
      Low_level.sleep_ms ~ms:delay_ms
    )
end

let secure_random = object
  inherit Eio.Flow.source
  method read_into buf = Low_level.arc4random buf; Cstruct.length buf
end

let stdenv ~run_queues ~run_event_loop =
  let of_unix fd = FD.of_unix_no_hook ~seekable:(FD.is_seekable fd) ~close_unix:true fd in
  let stdout = lazy (flow (of_unix Unix.stdout)) in
  let stderr = lazy (flow (of_unix Unix.stderr)) in
  let stdin = lazy (flow (of_unix Unix.stdin)) in
  object (_ : stdenv)
    method stdin  = (Lazy.force stdin :> Flow.source)
    method stdout = (Lazy.force stdout :> Flow.sink)
    method stderr = (Lazy.force stderr :> Flow.sink)
    method net = net
    method domain_mgr = domain_mgr ~run_queues ~run_event_loop
    method clock = clock
    method mono_clock = mono_clock
    method fs = (fs :> Eio.Fs.dir), "."
    method cwd = (cwd :> Eio.Fs.dir), "."
    method secure_random = secure_random
    method debug = Eio.Private.Debug.v
  end

let no_fallback (`Msg msg) = failwith msg

let run_queues =
  Eio.Idle_domains.prepare 4;
  Hashtbl.create 16

let rec run : type a. (_ -> a) -> a = fun main ->
  let stdenv = stdenv ~run_queues ~run_event_loop:run in
  let run_q = Lf_queue.create () in
  Lf_queue.push run_q IO;
  let kqueue = Kq.create () in
  let r, w = Unix.pipe () in
  let read_pipe = FD.of_unix_no_hook ~seekable:false ~close_unix:true r in
  let write_pipe = FD.of_unix_no_hook ~seekable:false ~close_unix:true w in
  let st = { run_q; kqueue; pending_io = Atomic.make 0; read_pipe; write_pipe; need_wakeup = Atomic.make false } in
  let rec fork ~new_fiber:fiber fn =
    let open Effect.Deep in
    Ctf.note_switch (Fiber_context.tid fiber);
    match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; schedule st);
        exnc = (fun ex ->
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex (Printexc.get_raw_backtrace ())
          );
        effc = fun (type a) (e : a Effect.t) ->
          match e with
          | Enter fn -> Some (fun k ->
              match Fiber_context.get_error fiber with
              | Some e -> discontinue k e
              | None ->
                let k = { Suspended.k; fiber } in
                fn st k;
                schedule st
            )
          | Close fd -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              enqueue_thread st k (Unix.close fd; 0);
              schedule st
            )
          | Eio.Private.Effects.Get_context -> Some (fun k -> continue k fiber)
          | Eio.Private.Effects.Suspend f -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              f fiber (function
                  | Ok v -> enqueue_thread st k v
                  | Error ex -> enqueue_failed_thread st k ex
                );
              schedule st
            )
          | Eio.Private.Effects.Fork (new_fiber, f) -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              enqueue_at_head st k ();
              fork ~new_fiber f
            )
          | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k mono_clock)
          | Cancel (job, exn, event) -> Some (fun k ->
              let ident = Kq.Events.get_ident event in
              let data = Kq.Events.get_data event in
              let filter = Kq.Events.get_filter event in
              let flags = Kqueue.Flag.delete in
              Kq.cancel ~filter ~data ~flags ~ident st.kqueue;
              let io_entry = Heap.ptr job |> Heap.free st.kqueue.heap in
              discontinue_io st exn io_entry;
              Atomic.decr st.pending_io;
              continue k ()
            )
          | Eio_unix.Private.Socket_of_fd (sw, close_unix, fd) -> Some (fun k ->
              let fd = FD.of_unix ~sw ~seekable:false ~close_unix fd in
              continue k (flow fd :> Eio_unix.socket)
            )
          | Eio_unix.Private.Socketpair (sw, domain, ty, protocol) -> Some (fun k ->
              let a, b = Unix.socketpair ~cloexec:true domain ty protocol in
              let a = FD.of_unix ~sw ~seekable:false ~close_unix:true a |> flow in
              let b = FD.of_unix ~sw ~seekable:false ~close_unix:true b |> flow in
              continue k ((a :> Eio_unix.socket), (b :> Eio_unix.socket))
            )
          | Eio_unix.Private.Pipe sw -> Some (fun k ->
              let r, w = Unix.pipe ~cloexec:true () in
              (* See issue #319, PR #327 *)
              Unix.set_nonblock r;
              Unix.set_nonblock w;
              let r = (flow (FD.of_unix ~sw ~seekable:false ~close_unix:true r) :> <Eio.Flow.source; Eio.Flow.close; Eio_unix.unix_fd>) in
              let w = (flow (FD.of_unix ~sw ~seekable:false ~close_unix:true w) :> <Eio.Flow.sink; Eio.Flow.close; Eio_unix.unix_fd>) in
              continue k (r, w)
            )
          | _ -> None
      }
  in
  let result = ref None in
  let `Exit_scheduler =
    let new_fiber = Fiber_context.make_root () in
    fork ~new_fiber (fun () ->
        Switch.run_protected (fun sw ->
            Switch.on_release sw (fun () -> ()
            (* TODO: Probably a pipe pool or something *)
            (* let unix = FD.to_unix `Take st.eventfd in
               EventFD_pool.put unix *)
                                 );
            result := Some (
                Fiber.first
                  (fun () -> main stdenv)
                  (fun () -> monitor_event_fd st)
              )
          )
      )
  in
  Kq.close st.kqueue;
  Option.get !result
