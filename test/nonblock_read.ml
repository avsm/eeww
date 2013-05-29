open Printf
open Lwt
open Lwt_unix

let test () =
  (* open tun *)
  let devname = "tap0" in
  printf "open %s: " devname;
  let fd, name = Tuntap.opentun ~devname () in
  printf "ok hwaddr: %s\n%!" (Tuntap.string_of_hwaddr (Tuntap.get_hwaddr devname));
  let ipv4 = "172.168.1.1" in
  let netmask = "255.255.255.0" in
  printf "%s <- %s/%s: " devname ipv4 netmask;
  Tuntap.set_ipv4 ~devname ~ipv4 ~netmask ();
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

let () = run (sleep 0.1 >>= test)
