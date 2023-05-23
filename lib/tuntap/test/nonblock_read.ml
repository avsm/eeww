open Printf
open Lwt
open Lwt_unix

let test devname =
  printf "open %s: " devname;
  let fd, _name =
    if String.sub devname 0 3 = "tun"
    then Tuntap.opentun ~devname ()
    else Tuntap.opentap ~devname () in
  printf "ok hwaddr: %s\n%!" (Macaddr.to_string (Tuntap.get_macaddr devname));
(*
  let ipv4 = "172.168.1.1" in
  let netmask = "255.255.255.0" in
  printf "%s <- %s/%s: " devname ipv4 netmask;
  Tuntap.set_ipv4 ~devname ~ipv4 ~netmask ();
*)
  Tuntap.set_up_and_running devname;
  printf "ok\n%!";
  let lfd = of_unix_file_descr fd ~blocking:false in
  let rec loop () =
    let buf = Lwt_bytes.create 4096 in
    handle_unix_error (Lwt_bytes.read lfd buf 0) (Lwt_bytes.length buf)
    >>= fun len ->
    printf "read: %d bytes\n%!" len;
    let packet = Lwt_bytes.extract buf 0 len in
    Lwt_io.hexdump Lwt_io.stdout (Lwt_bytes.to_string packet)
    >>= loop
  in loop ()

let () = if Array.length Sys.argv < 2
  then Printf.eprintf "Usage: %s <ifname>\n" Sys.argv.(0)
  else Lwt_main.run (sleep 0.1 >>= fun () -> test Sys.argv.(1))
