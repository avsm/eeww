module Server_state = struct
  type state =
    { buf : bytes
    ; mutable to_write : int
    }

  let init () = Hashtbl.create 128

  let add_client t fd =
    if not (Hashtbl.mem t fd)
    then (
      Unix.set_nonblock fd;
      Hashtbl.add t fd { buf = Bytes.create 1024; to_write = 0 })
  ;;

  let remove_client t fd =
    Unix.close fd;
    Hashtbl.remove t fd
  ;;

  let perform_read t fd =
    let state = Hashtbl.find t fd in
    match Unix.read fd state.buf 0 1024 with
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
      `Nothing_available
    | exception
        Unix.Unix_error
          ( ( EPIPE
            | ECONNRESET
            | EHOSTUNREACH
            | ENETDOWN
            | ENETRESET
            | ENETUNREACH
            | ETIMEDOUT )
          , _
          , _ ) -> `Eof
    | exception exn -> raise exn
    | 0 -> `Eof
    | n ->
      assert (n > 0);
      for i = 0 to n - 1 do
        if Bytes.unsafe_get state.buf i = '\n' then state.to_write <- state.to_write + 1
      done;
      `Read_some
  ;;

  let payload = Bytes.of_string "+PONG\r\n"

  let write t fd =
    let state = Hashtbl.find t fd in
    let len = Bytes.length payload in
    try
      while state.to_write > 0 do
        assert (Unix.write fd payload 0 len = len);
        state.to_write <- state.to_write - 1
      done;
      `Ok
    with
    | Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) -> `Ok
    | Unix.Unix_error
        ( ( EPIPE
          | ECONNRESET
          | EHOSTUNREACH
          | ENETDOWN
          | ENETRESET
          | ENETUNREACH
          | ETIMEDOUT )
        , _
        , _ ) -> `Eof
    | exn -> raise exn
  ;;
end

let accept_loop state socket k =
  try
    let client_fd, _ = Unix.accept ~cloexec:true socket in
    Server_state.add_client state client_fd;
    Kqueue.add k client_fd `Read_write;
    Kqueue.add k socket `Read
  with
  | Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> Kqueue.add k socket `Read
  | exn -> raise exn
;;

let server_loop socket k =
  let state = Server_state.init () in
  let rec run () =
    match Kqueue.wait k Kqueue.Timeout.immediate with
    | `Timeout -> run ()
    | `Ok ->
      Kqueue.iter_ready k ~f:(fun fd flags event ->
          if Kqueue.Flag.(is_subset ev_eof ~of_:flags)
          then Unix.close fd
          else if socket = fd
          then accept_loop state socket k
          else (
            match event with
            | `Write ->
              (match Server_state.write state fd with
              | `Ok -> ()
              | `Eof -> Server_state.remove_client state fd)
            | `Read ->
              let rec aux () =
                match Server_state.perform_read state fd with
                | `Nothing_available -> ()
                | `Eof -> Server_state.remove_client state fd
                | `Read_some -> aux ()
              in
              aux ();
              Kqueue.add k fd `Read_write));
      run ()
  in
  run ()
;;

let run sock_path =
  let delete_sock path =
    try Unix.unlink path with
    | Unix.Unix_error (ENOENT, _, _) -> ()
  in
  delete_sock sock_path;
  let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_UNIX sock_path);
  at_exit (fun () -> delete_sock sock_path);
  let k = Kqueue.kqueue ~changelist_size:128 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close socket;
      Kqueue.close k)
    (fun () ->
      Unix.set_nonblock socket;
      Unix.listen socket 128;
      Kqueue.add k socket `Read;
      server_loop socket k)
;;

let () = run Sys.argv.(1)
