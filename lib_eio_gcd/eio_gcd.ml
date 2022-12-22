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

 let src = Logs.Src.create "eio_gcd" ~doc:"Eio backend using gcd and luv"
 module Log = (val Logs.src_log src : Logs.LOG)

 open Eio.Std
 open Eio.Private
 open Eio_utils

 module Ctf = Eio.Private.Ctf

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

 module Suspended = struct
  type 'a t = {
    fiber : Eio.Private.Fiber_context.t;
    k : ('a, unit) Effect.Deep.continuation;
  }

  let tid t = Eio.Private.Fiber_context.tid t.fiber

  let continue t v =
    Ctf.note_switch (tid t);
    Effect.Deep.continue t.k v

  let discontinue t ex =
    Ctf.note_switch (tid t);
    Effect.Deep.discontinue t.k ex

  let continue_result t = function
    | Ok x -> continue t x
    | Error x -> discontinue t x
end


type runnable =
  | IO
  | Thread of string * (unit -> unit)

type t = {
  async : Dispatch.Queue.t;       (* Will process [run_q] when prodded. *)
  async_run : unit -> unit;
  run_q : runnable Lf_queue.t;
}

type _ Effect.t += Enter : (t -> 'a Suspended.t -> unit) -> 'a Effect.t

let enter fn = Effect.perform (Enter fn) 

let enqueue_thread ~msg t k v =
  Lf_queue.push t.run_q (Thread (msg, fun () -> Suspended.continue k v));
  Dispatch.async t.async t.async_run

let enqueue_result_thread ~msg t k r =
  Lf_queue.push t.run_q (Thread (msg, fun () -> Suspended.continue_result k r));
  Dispatch.async t.async t.async_run

let enqueue_failed_thread ~msg t k ex =
  Lf_queue.push t.run_q (Thread (msg, fun () -> Suspended.discontinue k ex));
  Dispatch.async t.async t.async_run

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
     Ctf.label "close";
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
      let fd = Dispatch.Io.get_unix io_channel |> Option.get in
      match op with
        | `Peek -> fd
        | `Take ->
          t.fd <- `Closed;
          Eio.Switch.remove_hook t.release_hook;
          fd
        

    (* Dispatch Queue
     Ideally, I think, this would be [Concurrent] but then the OCaml
     runtime lock would block the thread and cause GCD to "thread explode".

     BUG: Something in ocaml-dispatch needs to change with the retention/allocation of
     queues, if this `lazy` is removed then this is initialised when the module is and
     sometimes causes segfaults *)
   let io_queue = lazy Dispatch.Queue.(create ~typ:Serial ())

   let realpath p = try Ok (Unix.realpath p) with exn -> Error exn

   let mkdir ~mode p = try Ok (Unix.mkdir p mode) with exn -> Error exn

   (* We use `Unix.openfile` here because dispatch tries to be clever otherwise and will not open a
      file descriptor until the first IO operation. *)
   let open_ ~sw ?mode path flags =
    let create path =
        try
          let fd = Unix.openfile path flags (Option.value ~default:0 mode) in
          let io_channel = Dispatch.Io.create Stream (Dispatch.Io.Fd.of_unix fd) (Lazy.force io_queue) in
          Ok (of_gcd ~sw io_channel)
        with
        | exn -> Error exn
    in
   match Filename.is_relative path with
   | false -> create path
   | true -> match realpath Filename.current_dir_name with
     | Ok parent -> create Filename.(concat parent path)
     | Error _ as e -> e

   (* A read can call the callback multiple times with more and more chunks of
     data -- here we concatenate them and only once the read is finished reading
     do we enqueue thread. *)
    let read ~off ~length fd (buf : Buffer.t) =
      let read = ref 0 in
      enter (fun t k ->
        Dispatch.Io.with_read ~off ~length ~queue:(Lazy.force io_queue) (get "with_read" fd) ~f:(fun ~err ~finished r ->
          let size = Dispatch.Data.size r in
          if err = 0 && finished then begin
            if size = 0 then begin
              enqueue_thread ~msg:"read of zero" t k !read
            end
            else (
              read := !read + size;
              buf := Dispatch.Data.concat r !buf;
              enqueue_thread ~msg:"read not zero" t k !read
            )
          end
          else if err <> 0 then enqueue_failed_thread ~msg:"failed read" t k (Failure "Read failed")
          else begin
            read := !read + size;
            buf := Dispatch.Data.concat r !buf
          end
        )
      )

   let write fd (bufs : Buffer.t) =
    enter (fun t k ->
        Dispatch.Io.with_write ~off:0 ~data:(!bufs) ~queue:(Lazy.force io_queue) (get "with_write" fd) ~f:(fun ~err ~finished _remaining ->
            if err <> 0 then enqueue_failed_thread ~msg:"failed write" t k (Failure "Write failed") else 
            if finished then enqueue_thread ~msg:"write finished" t k ()
            else ()
          )
      )

  (* TODO: This isn't write and segfaults! *)
  let _fast_copy src dst =
    let data = Buffer.empty () in
    let off = ref 0 in
    try
      while true do
        let got = read ~off:!off ~length:max_int src data in
        off := !off + got;
        write dst data
      done
    with End_of_file -> ()
 end

 module Conn = struct
    let receive ?(max=max_int) (conn : Network.Connection.t) buf =
      let r = enter (fun t k ->
        Network.Connection.receive ~min:0 ~max conn ~completion:(fun data _context is_complete err ->
          match data with
            | None -> enqueue_thread ~msg:"recv no data" t k (Ok (0, true))
            | Some data ->
              let err_code = Network.Error.to_int err in
              let res =
                if err_code = 0 then begin
                  let size = Dispatch.Data.size data in
                  Buffer.concat buf data;
                  Ok (size, is_complete)
                  end else Error (`Msg (string_of_int err_code))
                in
              enqueue_thread ~msg:"recv data" t k res
        )
      ) in match r with
      | Ok (0, true) -> raise End_of_file
      | Ok (got, true) -> got
      | Ok (got, false) -> got
      | Error (`Msg e) -> failwith ("Connection receive failed with " ^ e)

    let send (conn : Network.Connection.t) buf =
      enter (fun t k ->
        Network.Connection.send ~is_complete:true ~context:Default conn ~data:(!buf) ~completion:(fun e ->
          match Network.Error.to_int e with
          | 0 -> enqueue_thread ~msg:"send none" t k (Ok ())
          | i -> enqueue_thread ~msg:"send err" t k (Error (`Msg (string_of_int i)))
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
            Logs.debug (fun f -> f "Sending 'write close' to connection");
            enqueue_thread ~msg:"shutdown write" t k (Ok ())
          | i ->
            Logs.debug (fun f -> f "Got error %i" i);
            enqueue_thread ~msg:"shutdown err" t k (Error (`Msg (string_of_int i)))
        )
      ) in match r with
          | Ok () -> ()
          | Error (`Msg m) -> failwith m
     )
     | `Receive -> failwith "shutdown receive not supported"
     | `All ->
       Log.warn (fun f -> f "shutdown receive not supported")
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
    Switch.on_release sw (fun () -> (* TODO *) ());
    let (conn, sockaddr) = Option.get conn_sock in
    (socket conn), sockaddr

  initializer
    let handler (state : Network.Listener.State.t) err =
      match (Network.Error.to_int err, state) with
        | i, _ when i <> 0 -> failwith ("Listener failed with error code: " ^ string_of_int i)
        | _, Ready -> Log.debug (fun f -> f "Listening on port %i..." (Network.Listener.get_port sock));
        | _, Failed -> failwith "Network listener failed"
        | _, Invalid -> Logs.debug (fun f -> f "Listener changed to invalid state")
        | _, Waiting ->Logs.debug (fun f -> f "Listener is waiting...")
        | _, Cancelled -> Logs.debug (fun f -> f "Listener is cancelled")
  in
   let conn_handler conn =
    Eio.Semaphore.release connected;
    Network.Connection.retain conn;
    Network.Connection.set_queue ~queue:net_queue conn;
    Network.Connection.start conn;
    let endpoint = Network.Connection.copy_endpoint conn in
    let sockaddr = self#get_endpoint_addr endpoint in
    conn_sock <- Some (conn, sockaddr)
    in
      Logs.debug (fun f -> f "Initialising socket");
      Network.Listener.set_state_changed_handler ~handler sock;
      Network.Listener.set_new_connection_handler ~handler:conn_handler sock;
      Network.Listener.start sock


end
  let listening_socket ~backlog net_queue sock = object
    inherit [[ `TCP ]] listening_socket ~backlog net_queue sock

    method private get_endpoint_addr e =
      match Option.get @@ Network.Endpoint.get_address e with
        | Unix.ADDR_UNIX path         -> `Unix path
        | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Ipaddr.of_unix host, port) (* TODO: Remove unix *)
    end

   let net = object
     inherit Eio.Net.t

     val queue = Dispatch.Queue.create ~typ:Serial ()

     method datagram_socket = failwith "TODO: datagram socket"
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
         let listener = Listener.create params in
         Listener.set_queue ~queue listener;
         Listener.retain listener;
         listening_socket ~backlog queue listener
       | _ -> assert false

     method connect ~sw:_ = function
       | `Tcp (hostname, port) ->
          Logs.debug (fun f -> f "Connecting...");
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
          let handler t k (state : Network.Connection.State.t) _ =
            match state with
            | Waiting -> 
              Logs.debug (fun f -> f "Connection is waiting...")
            | Ready ->
              Logs.debug (fun f -> f "Connection is ready");
              enqueue_thread ~msg:"connection" t k (socket connection)
            | Invalid -> Logs.warn (fun f -> f "Invalid connection")
            | Preparing -> Logs.debug (fun f -> f "Connection is being prepared")
            | Failed -> Logs.warn (fun f -> f "Connection failed")
            | Cancelled -> Logs.debug (fun f -> f "Connection has been cancelled")
          in
          enter (fun t k ->
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

  method probe : type a. a Eio.Generic.ty -> a option = function
    | FD -> Some fd
    | Eio_unix.Private.Unix_file_descr op -> Some (File.to_unix op fd)
    | _ -> None

  method read_into buff =
    let data = Buffer.empty () in
    let res = File.read ~off:0 ~length:(buff.len) fd data in
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
          File.write fd (ref @@ Dispatch.Data.create (Cstruct.to_bigarray chunk))
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

  method open_in ~sw path =
    let fd = File.open_ ~sw (self#resolve path) [ Unix.O_RDONLY ] |> or_raise_fs in
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
    let fd = File.open_ ~sw real_path flags ~mode |> or_raise_fs in
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

let clock = object
  inherit Eio.Time.clock
  val queue = Dispatch.Queue.create ()

  method now = Unix.gettimeofday ()

  method sleep_until due =
    let delay = 1_000_000_000. *. (due -. Unix.gettimeofday ()) |> ceil |> Int64.of_float |> Int64.max 0L in
    enter @@ fun t k ->
    (* TODO: Dispatch work items are cancellable! *)
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
      (* Luv.Handle.close timer (fun () -> ()); *)
      enqueue_failed_thread ~msg:"Timeout stopped!" t k ex
    );
    Dispatch.after ~delay queue (fun () -> 
      match Fiber_context.get_error k.fiber with
      | None -> enqueue_thread ~msg:"Sleepy" t k ()
      | Some _ -> ())
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

let stdenv =
  let stdin = lazy (source (File.of_gcd_no_hook @@ Dispatch.Io.(create Stream Fd.stdin (Lazy.force File.io_queue)))) in
  let stdout = lazy (sink (File.of_gcd_no_hook @@ Dispatch.Io.(create Stream Fd.stdout (Lazy.force File.io_queue)))) in
  let stderr = lazy (sink (File.of_gcd_no_hook @@ Dispatch.Io.(create Stream Fd.stderr (Lazy.force File.io_queue)))) in
  object (_ : stdenv)
    method stdin  = (Lazy.force stdin)
    method stdout = (Lazy.force stdout)
    method stderr = (Lazy.force stderr)
    method net = net
    method fs = (fs :> Eio.Fs.dir), "."
    method cwd = (cwd :> Eio.Fs.dir), "."
    method secure_random = secure_random
    method clock = clock
    method mono_clock = failwith "Mono unimplemented"
    method debug = Eio.Private.Debug.v
    method domain_mgr = failwith "Domain Manager unimplemented"
  end

let rec wakeup ~async ~io_queued run_q =
  (* Eio.traceln "HERE!"; *)
  match Lf_queue.pop run_q with
  | Some (Thread (_msg, f)) ->
    (* Eio.traceln "Scheduler: %s" msg; *)
    if not !io_queued then (
      Lf_queue.push run_q IO;
      io_queued := true;
    );
    f ();
    wakeup ~async ~io_queued run_q
  | Some IO ->
    (* If threads keep yielding they could prevent pending IO from being processed.
       Therefore, we keep an [IO] job on the queue to force us to check from time to time. *)
    io_queued := false;
    if not (Lf_queue.is_empty run_q) then
      Dispatch.async async (fun () -> wakeup ~async ~io_queued run_q)
  | None ->
    ()

let enqueue_at_head t k v =
  Lf_queue.push_head t.run_q (Thread ("FORK", fun () -> Suspended.continue k v));
  Dispatch.async t.async t.async_run

let run : type a. (_ -> a) -> a = fun main ->
  let open Eio.Private in
  Log.debug (fun l -> l "starting run");
  let run_q = Lf_queue.create () in
  let io_queued = ref false in
  let async = Dispatch.Queue.create () in
  let st = { async; async_run = (fun () -> wakeup ~async ~io_queued run_q); run_q } in
  let rec fork ~new_fiber:fiber fn =
    Ctf.note_switch (Fiber_context.tid fiber);
    let open Effect.Deep in
    match_with fn ()
    { retc = (fun () -> Fiber_context.destroy fiber);
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
            | None -> fn st { Suspended.k; fiber }
          )
        | Eio.Private.Effects.Suspend fn ->
          Some (fun k ->
              (* Eio.traceln "suspended"; *)
              let k = { Suspended.k; fiber } in
              fn fiber (enqueue_result_thread ~msg:"suspend" st k)
            )
        | Eio_unix.Private.Pipe _ -> Some (fun k ->
            discontinue k (Failure "Pipes not supported by GCD backend yet")
          )
        | Eio_unix.Private.Socketpair _ -> Some (fun k ->
            discontinue k (Failure "Socketpair not supported by GCD backend yet")
          )
        | _ -> None
    }
  in
  let main_status = ref `Running in
  let new_fiber = Fiber_context.make_root () in
  let finished = Dispatch.Group.create () in
  fork ~new_fiber (fun () ->
      Dispatch.Group.enter finished;
      begin match main stdenv with
        | v -> main_status := `Done v
        | exception ex -> main_status := `Ex (ex, Printexc.get_raw_backtrace ())
      end;
      Dispatch.Group.leave finished
    );
  let _ = Dispatch.Group.wait finished (Dispatch.Time.dispatch_forever ()) in
  (* Can't do this yet because cancellation isn't implemented everywhere *)
  (* Lf_queue.close st.run_q; *)
  match !main_status with
  | `Done v -> v
  | `Ex (ex, bt) -> Printexc.raise_with_backtrace ex bt
  | `Running -> failwith "Deadlock detected: no events scheduled but main function hasn't returned"
