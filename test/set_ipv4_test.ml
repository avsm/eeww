open Tuntap
open OUnit

let test_setipv4 ipv4 netmask () =
  let fd, dev = opentap ~devname:"tap0" () in
  set_ipv4 ~dev ~ipv4 ~netmask ();
  let iface_addr = List.assoc dev (getifaddrs ()) in
  assert_equal ipv4 (Cstruct.ipv4_to_string iface_addr.addr);
  assert_equal netmask (Cstruct.ipv4_to_string iface_addr.mask);
  Unix.close fd


let suite = "Test IPv4" >::: ["test_classA" >:: test_setipv4 "10.0.0.1" "255.0.0.0";
                              "test_classB" >:: test_setipv4 "172.16.0.1" "255.255.0.0";
                              "test_classC" >:: test_setipv4 "192.168.0.1" "255.255.255.0"]


let _ = run_test_tt_main suite
