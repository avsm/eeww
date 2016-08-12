(* this test is intended to mimic a user attempting to connect and disconnect
 * a tap interface. *)
let () =
  let devname = "tap0" in
  let fd, devname = Tuntap.opentap ~devname () in
  Tuntap.set_up_and_running devname;
  Printf.printf "set_up_and_running completed for %s\n" devname;
  Unix.close fd;
  Tuntap.closetap devname;
  Printf.printf "closetun completed for %s\n" devname
