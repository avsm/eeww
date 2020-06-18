open Printf
open Lwt
open Lwt_unix

let child socket =
  let io_vectors = IO_vectors.create () in
  handle_unix_error (fun () -> recv_msg ~socket ~io_vectors) ()
  >>= fun (_, fds) ->
  printf "received %d fds\n" (List.length fds);
  return ()

let parent socket =
  (* open tun *)
  let fd, name = Tuntap.opentun ~devname:"tap0" () in
  printf "parent open: %s\n" name;
  let io_vectors = IO_vectors.create () in
  handle_unix_error (fun () -> send_msg ~socket ~io_vectors ~fds:[fd]) ()
  >>= fun _ ->
  printf "parent sent\n";
  return ()

let t () =
  let psock,csock = socketpair PF_UNIX SOCK_STREAM 0 in
  match fork () with
  |(-1) -> printf "error\n"; exit 1
  |0 -> (* child *)
     close psock >>= fun () -> child csock
  |_pid -> (* parent *)
     close csock >>= fun () -> parent psock

let () = Lwt_main.run (sleep 0.1 >>= t)
