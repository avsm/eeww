open Ctypes
open Kqueue

let unsafe_fd_to_int (t : Unix.file_descr) : int = Obj.magic t

let unsafe_int_to_fd (t : int) : Unix.file_descr = Obj.magic t

let run socket q =
  let b = Bytes.create 4096 in
  let changelist = CArray.make Bindings.Kevent.t 0 in
  let eventlist = CArray.make Bindings.Kevent.t 1024 in
  let rec run () =
    let n = kevent q ~changelist ~eventlist in
    if n = -1 then (
      prerr_endline "Error with kevent" ;
      exit 1 ) ;
    for i = 0 to n - 1 do
      let event = CArray.get eventlist i in
      let ident = Kevent.ident event |> Uintptr.to_int in
      let flags = Kevent.flags event in
      (* Close if EOF *)
      if Unsigned.UInt16.(logand flags Bindings.ev_eof > zero) then
        Unix.close (unsafe_int_to_fd ident)
      else if unsafe_fd_to_int socket = ident then (
        let client_fd, _ = Unix.accept socket in
        Unix.set_nonblock client_fd ;
        let client_ident = unsafe_fd_to_int client_fd |> Uintptr.of_int in
        let e =
          Kevent.make ~ident:client_ident ~filter:Bindings.evfilt_read
            ~flags:(Unsigned.UInt16.logor Bindings.ev_add Bindings.ev_enable)
            ~fflags:Unsigned.UInt32.zero ~data:Intptr.zero ~udata:Uintptr.zero
        in
        let res =
          kevent q
            ~changelist:(CArray.of_list Bindings.Kevent.t [e])
            ~eventlist:(CArray.make Bindings.Kevent.t 0)
        in
        assert (res > -1) )
      else if Kevent.filter event = Bindings.evfilt_read then
        try
          let bc = Unix.read (unsafe_int_to_fd ident) b 0 4096 in
          if bc > 0 then ignore (Unix.write (unsafe_int_to_fd ident) b 0 bc)
        with
        | Unix.Unix_error (Unix.EAGAIN, _, _)
        | Unix.Unix_error (Unix.EWOULDBLOCK, _, _)
        ->
          (* we are in nonblocking mode, EAGAIN indicates we should try to read
             again *)
          ()
    done ;
    run ()
  in
  run ()

let () =
  let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close socket)
  @@ fun () ->
  Unix.set_nonblock socket ;
  Unix.setsockopt socket Unix.SO_REUSEADDR true ;
  let addr = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.bind socket (Unix.ADDR_INET (addr, 8080)) ;
  Unix.listen socket 11_000 ;
  print_endline "Listening on 127.0.0.1:8080" ;
  let q = kqueue () in
  let ev =
    Kevent.make
      ~ident:(unsafe_fd_to_int socket |> Uintptr.of_int)
      ~filter:Bindings.evfilt_read
      ~flags:(Unsigned.UInt16.logor Bindings.ev_add Bindings.ev_enable)
      ~fflags:Unsigned.UInt32.zero ~data:Intptr.zero ~udata:Uintptr.zero
  in
  let res =
    kevent q
      ~changelist:(CArray.of_list Bindings.Kevent.t [ev])
      ~eventlist:(CArray.make Bindings.Kevent.t 0)
  in
  assert (res == 0) ;
  run socket q
