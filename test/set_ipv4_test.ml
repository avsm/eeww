open Tuntap
open OUnit2

open Ipaddr

let test_setipv4 ipv4 netmask ctx =
  let ipv4 = V4.of_string_exn ipv4 in
  let netmask = V4.Prefix.make netmask ipv4 in
  let fd, devname = opentap ~devname:"tap0" () in
  set_ipv4 ~netmask devname ipv4;
  let iface_addr = List.hd @@ v4_of_ifname devname in
  assert_equal
    ~msg:(Printf.sprintf "%s %s" (V4.to_string ipv4) (V4.Prefix.to_string netmask))
    ~printer:(function (a, m) ->
        Printf.sprintf "%s %s" (V4.to_string a) (V4.Prefix.to_string m)
      )
    (ipv4, netmask) iface_addr;
  Unix.close fd

let suite = "Test IPv4" >:::
            ["test_classA" >:: test_setipv4 "10.0.0.1" 8;
             "test_classB" >:: test_setipv4 "172.16.0.1" 16;
             "test_classC" >:: test_setipv4 "192.168.0.1" 24;
             "test_wrongclassA" >:: test_setipv4 "10.11.12.1" 24;
             "test_wrongclassB" >:: test_setipv4 "172.16.0.1" 24;
             "test_wrongclassC" >:: test_setipv4 "192.168.1.2" 8;
             "test_noclass" >:: test_setipv4 "192.168.1.2" 23;
            ]

let _ = run_test_tt_main suite
