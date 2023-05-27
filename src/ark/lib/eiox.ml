open Eio

let stream_source q =
  object(self)
    val mutable data = []
    inherit Flow.source

    method read_into dst =
      let avail, src = Cstruct.fillv ~dst ~src:data in
      match avail with
      | 0 -> begin
          match Stream.take q with
          | None -> raise End_of_file
          | Some d ->
             data <- [d];
             self#read_into dst
      end
      | _ ->
        data <- src;
        avail
    end

let capnp_sink fn =
  object
    inherit Flow.sink

    method copy src =
      let buf = Cstruct.create 4096 in
      try while true do
        let got = src#read_into buf in
        let chunk = Cstruct.to_string ~len:got buf in
        match fn chunk with
        | Ok () -> ()
        | Error (`Capnp _) -> raise End_of_file
      done with End_of_file -> ()

    method! write bufs =
      List.iter (fun buf ->
        match fn (Cstruct.to_string buf) with
        | Ok () -> ()
        | Error (`Capnp _) -> raise End_of_file
      ) bufs
    end

let fork_pty_shell ~sw ~stdout ~stdin ~executable argv =
   let open Eio_unix in
   let pty = Pty.open_pty () in
   let terminal = Fd.of_unix ~sw ~close_unix:true pty.Pty.slavefd in
   let proc =
     let ptyAction = Eio_unix.Private.Fork_action.login_tty terminal
     and execvAction =
       Private.Fork_action.execve
       executable
       ~argv:(Array.of_list argv)
       ~env:(Unix.unsafe_environment ()) (* TODO filter env *)
     in
     Eio_posix.Low_level.Process.spawn ~sw [ ptyAction; execvAction ]
   in
   Fiber.fork ~sw (fun () ->
     Fiber.both
     (fun () ->
       let sink = Eio_unix.import_socket_stream ~sw ~close_unix:false pty.Eio_unix.Pty.masterfd in
       Flow.copy stdin sink)
     (fun () ->
       let source = Eio_unix.import_socket_stream ~sw ~close_unix:false pty.Eio_unix.Pty.masterfd in
       Flow.copy source stdout)
   );
   object
    method pid = Eio_posix.Low_level.Process.pid proc

    method await =
      match Eio.Promise.await @@ Eio_posix.Low_level.Process.exit_status proc with
      | Unix.WEXITED i -> `Exited i
      | Unix.WSIGNALED i -> `Signaled i
      | Unix.WSTOPPED _ -> assert false

    method signal i = Eio_posix.Low_level.Process.signal proc i
  end

(* Terminal stuff *)
let run env ~stdin ~stdout on_complete_t =
  Eio.Fiber.any [
   (fun () -> Eio.Flow.copy env#stdin stdin; 0l);
   (fun () -> Eio.Flow.copy stdout env#stdout; 0l);
   (fun () -> Eio.Promise.await on_complete_t)
  ]

let run_with_raw_term env ~stdin ~stdout on_complete_t =
  let savedTio = Unix.tcgetattr Unix.stdin in

  (* set raw mode *)
  let tio =
    {
      savedTio with
      (* input modes *)
      c_ignpar = true;
      c_istrip = false;
      c_inlcr = false;
      c_igncr = false;
      c_ixon = false;
      (* c_ixany = false; *)
      (* c_iuclc = false; *)
      c_ixoff = false;
      (* output modes *)
      c_opost = false;
      (* control modes *)
      c_isig = false;
      c_icanon = false;
      c_echo = false;
      c_echoe = false;
      c_echok = false;
      c_echonl = false;
      (* c_iexten = false; *)

      (* special characters *)
      c_vmin = 1;
      c_vtime = 0;
    }
  in
  Unix.tcsetattr Unix.stdin TCSADRAIN tio;
  (* TODO send window size change update https://www.ietf.org/rfc/rfc4254.html#section-6.7 *)
  (* handle window size change *)
  (* match Pty.get_sigwinch () with
     | None -> ()
     | Some sigwinch -> (
         let handle_sigwinch (_signum : int) =
           let ws = Pty.tty_window_size () in
           ignore (Pty.set_window_size pty ws)
         in
         handle_sigwinch sigwinch;
         ignore (Sys.signal sigwinch (Signal_handle handle_sigwinch))); *)
  let code = run env ~stdin ~stdout on_complete_t in
  (* TODO detect terminated session *)
  (* TODO use nagle's algorithm? *)
  (* restore tio *)
  Unix.tcsetattr Unix.stdin TCSADRAIN savedTio;
  code 
