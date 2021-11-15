module Buf = struct
  open Core

  type t =
    { mutable buf : (Bigstring.t[@sexp.opaque])
    ; mutable pos_read : int
    ; mutable pos_fill : int
    }
  [@@deriving sexp_of]

  let create size =
    let buf = Bigstring.create size in
    { buf; pos_read = 0; pos_fill = 0 }
  ;;

  let compact t =
    if t.pos_read > 0
    then (
      let len = t.pos_fill - t.pos_read in
      Bigstring.blit ~src:t.buf ~dst:t.buf ~src_pos:t.pos_read ~dst_pos:0 ~len;
      t.pos_read <- 0;
      t.pos_fill <- len)
  ;;

  let length t = t.pos_fill - t.pos_read

  let resize t size =
    let new_len = (Bigstring.length t.buf + size) * 2 in
    t.buf <- Bigstring.unsafe_destroy_and_resize t.buf ~len:new_len
  ;;

  let read_assume_fd_is_nonblocking fd t =
    let res =
      Bigstring_unix.read_assume_fd_is_nonblocking
        fd
        t.buf
        ~pos:t.pos_fill
        ~len:(Bigstring.length t.buf - t.pos_fill)
    in
    if Unix.Syscall_result.Int.is_ok res
    then (
      let count = Unix.Syscall_result.Int.ok_exn res in
      if count > 0 then t.pos_fill <- t.pos_fill + count);
    res
  ;;

  let write_assume_fd_is_nonblocking fd t =
    let res =
      Bigstring_unix.write_assume_fd_is_nonblocking
        fd
        t.buf
        ~pos:t.pos_read
        ~len:(length t)
    in
    if res > 0 then t.pos_read <- t.pos_read + res;
    res
  ;;

  let available_to_write t = Bigstring.length t.buf - t.pos_fill

  module Consume = struct
    let unsafe_bigstring t ~f =
      let len = length t in
      let count = f t.buf ~pos:t.pos_read ~len in
      if count < 0 || count > len
      then
        invalid_argf "Bytebuffer.consume: Invalid value for consumed count - %d" count ();
      t.pos_read <- t.pos_read + count
    ;;
  end

  module Fill = struct
    let add_gen t ?pos ?len ~total_length ~blit str =
      let src_pos, len =
        Ordered_collection_common.get_pos_len_exn ?pos ?len () ~total_length
      in
      if available_to_write t < len then resize t len;
      blit ~src:str ~src_pos ~dst:t.buf ~dst_pos:t.pos_fill ~len;
      t.pos_fill <- t.pos_fill + len
    ;;

    let string t ?pos ?len str =
      add_gen
        t
        ?pos
        ?len
        ~total_length:(String.length str)
        ~blit:Bigstring.From_string.blit
        str
    ;;
  end
end

module Server_state = struct
  open Core

  type state =
    { read_buf : Buf.t
    ; write_buf : Buf.t
    }

  let init () =
    Bounded_int_table.create
      ~sexp_of_key:Unix.File_descr.sexp_of_t
      ~num_keys:1024
      ~key_to_int:Unix.File_descr.to_int
      ()
  ;;

  let add_client t fd =
    if not (Bounded_int_table.mem t fd)
    then (
      Unix.set_nonblock fd;
      Bounded_int_table.add_exn
        t
        ~key:fd
        ~data:{ read_buf = Buf.create 1024; write_buf = Buf.create 1024 })
  ;;

  let remove_client t fd = Bounded_int_table.remove t fd

  let perform_read t fd =
    let state = Bounded_int_table.find_exn t fd in
    Buf.compact state.read_buf;
    let result = Buf.read_assume_fd_is_nonblocking fd state.read_buf in
    if Unix.Syscall_result.Int.is_ok result
    then (
      match Unix.Syscall_result.Int.ok_exn result with
      | 0 -> `Eof
      | n ->
        assert (n > 0);
        `Read_some)
    else (
      match Unix.Syscall_result.Int.error_exn result with
      | EAGAIN | EWOULDBLOCK | EINTR -> `Nothing_available
      | EPIPE | ECONNRESET | EHOSTUNREACH | ENETDOWN | ENETRESET | ENETUNREACH | ETIMEDOUT
        -> `Eof
      | error -> raise (Unix.Unix_error (error, "read", "")))
  ;;

  let write t fd =
    let state = Bounded_int_table.find_exn t fd in
    match Buf.write_assume_fd_is_nonblocking fd state.write_buf with
    | n ->
      assert (n >= 0);
      Buf.compact state.write_buf;
      `Ok
    | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) -> `Ok
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
  ;;

  let with_state t fd ~f =
    let state = Bounded_int_table.find_exn t fd in
    f ~read_buf:state.read_buf ~write_buf:state.write_buf
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
                | `Read_some ->
                  Server_state.with_state state fd ~f:(fun ~read_buf ~write_buf ->
                      let payload = "+PONG\r\n" in
                      Buf.Consume.unsafe_bigstring read_buf ~f:(fun buf ~pos ~len ->
                          for i = pos to len - 1 do
                            if Core.Bigstring.get buf i = '\n'
                            then Buf.Fill.string write_buf payload
                          done;
                          len));
                  aux ()
              in
              aux ();
              Kqueue.add k fd `Read_write));
      run ()
  in
  run ()
;;

let run sock_path =
  let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let k = Kqueue.kqueue ~changelist_size:256 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close socket;
      Kqueue.close k)
    (fun () ->
      Unix.set_nonblock socket;
      Unix.bind socket (Unix.ADDR_UNIX sock_path);
      Unix.listen socket 11_000;
      Kqueue.add k socket `Read;
      server_loop socket k)
;;

let () = run Sys.argv.(1)
