open Ipaddr
open Tuntap
open OUnit

let test_setipv4 ipv4 netmask () =
  let ipv4 = of_string_exn ipv4 in
  let fd, devname = opentap ~devname:"tap0" () in
  set_ipaddr ~netmask devname ipv4;
  let iface_addr = List.find
      (fun ifaddr -> ifaddr.name = devname
                     && match ifaddr.ipaddr with AF_INET _ -> true | _ -> false)
      (getifaddrs ())
  in
  assert_equal (ipv4, netmask) (match iface_addr.ipaddr with
      | AF_INET (a, p) -> V4 a, V4.Prefix.bits p
      | _ -> assert false);

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
