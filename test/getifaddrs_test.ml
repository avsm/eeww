open Tuntap
open Ipaddr

let () =
  let addrs = getifaddrs () in
  List.iter (function
      | `V4 (n, a, p) ->
        Printf.printf "%s -> %s/%d\n"
          n (V4.to_string a) (V4.Prefix.bits p)
      | `V6 (n, a, p) ->
        Printf.printf "%s -> %s/%d\n"
          n (V6.to_string ~v4:false a) (V6.Prefix.bits p)
    ) addrs
