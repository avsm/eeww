open Tuntap
open Cstruct

let () =
  let addrs = getifaddrs () in
  List.iter (fun (n, ifa) ->
    Printf.printf "%s -> %s, %s, %s\n%!"
      n (ipv4_to_string ifa.addr) (ipv4_to_string ifa.mask) (ipv4_to_string ifa.brd)
  ) addrs
