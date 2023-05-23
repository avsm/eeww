open Tuntap
open Ipaddr

let () =
  let addrs = getifaddrs () in
  List.iter (function
      | n, `V4 cidr ->
        Printf.printf "%s -> %s\n" n (V4.Prefix.to_string cidr)
      | n, `V6 cidr ->
        Printf.printf "%s -> %s\n" n (V6.Prefix.to_string cidr)
    ) addrs
