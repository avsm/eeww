open Tuntap
module Ipv4 = Ipaddr.V4

let () =
  let addrs = getifaddrs () in
  List.iter (fun (n, ifa) ->
    Printf.printf "%s -> %s, %s, %s\n%!" n
      (Ipv4.to_string ifa.addr)
      (Ipv4.to_string ifa.mask)
      (Ipv4.to_string ifa.brd)
  ) addrs
