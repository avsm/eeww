open Tuntap
open Ipaddr

let () =
  let addrs = getifaddrs () in
  List.iter (fun ifaddr ->
      Printf.printf "%s -> %s/%d\n%!"
        ifaddr.name
        (to_string ifaddr.ipaddr)
        ifaddr.netmask
    ) addrs
