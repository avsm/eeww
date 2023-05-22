open Printf
open Lwt
open Lwt_unix

let test () =
  (* open tun *)
  let fd, name = Tuntap.opentun ~devname:"tap0" () in
  printf "parent open: %s\n" name;
  let _lfd = of_unix_file_descr fd ~blocking:false in
  sleep 0.1

let () = Lwt_main.run (sleep 0.1 >>= test)
