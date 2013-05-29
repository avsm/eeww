open Tuntap
open OUnit

let test_setipv4 ipv4 netmask () =
  let fd, devname = opentap ~devname:"tap0" () in
  set_ipv4 ~devname ~ipv4 ~netmask ();
  let iface_addr = List.assoc devname (getifaddrs ()) in
  let printer = fun s -> s in
  assert_equal ~printer ipv4 (Cstruct.ipv4_to_string iface_addr.addr);
  assert_equal ~printer netmask (Cstruct.ipv4_to_string iface_addr.mask);
  Unix.close fd


let suite = "Test IPv4" >::: ["test_classA" >:: test_setipv4 "10.0.0.1" "255.0.0.0";
                              "test_classB" >:: test_setipv4 "172.16.0.1" "255.255.0.0";
                              "test_classC" >:: test_setipv4 "192.168.0.1" "255.255.255.0";
                              "test_wrongclassA" >:: test_setipv4 "10.11.12.1" "255.255.255.0";
                              "test_wrongclassB" >:: test_setipv4 "172.16.0.1" "255.255.255.0";
                              "test_wrongclassC" >:: test_setipv4 "192.168.1.2" "255.0.0.0";
                              "test_noclass" >:: test_setipv4 "192.168.1.2" "255.255.254.0"
                             ]


let _ = run_test_tt_main suite
