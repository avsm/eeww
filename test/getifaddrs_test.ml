open Tuntap
open Ipaddr

let () =
  let addrs = getifaddrs () in
  List.iter (function
      | n, `V4 (a, p) ->
        Printf.printf "%s -> %s/%d\n"
          n (V4.to_string a) (V4.Prefix.bits p)
      | n, `V6 (a, p) ->
        Printf.printf "%s -> %s/%d\n"
          n (V6.to_string ~v4:false a) (V6.Prefix.bits p)
    ) addrs
