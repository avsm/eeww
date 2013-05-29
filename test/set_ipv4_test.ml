open Tuntap
open OUnit

let test_setipv4 ipv4 netmask () =
  let fd, devname = opentap ~devname:"tap0" () in
  set_ipv4 ~devname ~ipv4 ~netmask ();
  let iface_addr = List.assoc devname (getifaddrs ()) in
  assert_equal ipv4 (Cstruct.ipv4_to_string iface_addr.addr);
  assert_equal netmask (Cstruct.ipv4_to_string iface_addr.mask);
  Unix.close fd


let suite = "Test IPv4" >::: ["test_classA" >:: test_setipv4 "10.0.0.1" "255.0.0.0";
                              "test_classB" >:: test_setipv4 "172.16.0.1" "255.255.0.0";
                              "test_classC" >:: test_setipv4 "192.168.0.1" "255.255.255.0";
                              "test_anil_problem" >:: test_setipv4 "10.11.12.1" "255.255.255.0"
                             ]


let _ = run_test_tt_main suite
