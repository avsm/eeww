# Setting up the environment

```ocaml
# #require "eio_linux";;
```

```ocaml
open Eio.Std

let ( / ) = Eio.Path.( / )
```

Sending a file descriptor over a Unix domain socket:

```ocaml
# Eio_linux.run @@ fun env ->
  Switch.run @@ fun sw ->
  let fd = Eio.Path.open_out ~sw (env#cwd / "tmp.txt") ~create:(`Exclusive 0o600) in
  Eio.Flow.copy_string "Test data" fd;
  let r, w = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let r = Eio_unix.Fd.of_unix ~sw ~seekable:false ~close_unix:true r in
  let w = Eio_unix.Fd.of_unix ~sw ~seekable:false ~close_unix:true w in
  Fiber.both
    (fun () ->
       let sent = Eio_linux.Low_level.send_msg w [Cstruct.of_string "x"] ~fds:[Eio_unix.Resource.fd_opt fd |> Option.get] in
       assert (sent = 1)
    )
    (fun () ->
       let buf = Cstruct.of_string "?" in
       let addr, got, fds = Eio_linux.Low_level.recv_msg_with_fds ~sw r ~max_fds:10 [buf] in
       traceln "Got: %S plus %d FD" (Cstruct.to_string buf) (List.length fds);
       match fds with
       | [fd] ->
         Eio_unix.Fd.use_exn "read" fd @@ fun fd ->
         ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
         traceln "Read: %S" (really_input_string (Unix.in_channel_of_descr fd) 9);
       | _ -> assert false
    );;
+Got: "x" plus 1 FD
+Read: "Test data"
- : unit = ()
```
