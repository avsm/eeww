open Tuntap
open Ipaddr

let () =
  let addrs = getifaddrs () in
  List.iter (fun ifaddr ->
      Printf.printf "%s -> " ifaddr.name;
      let addr, bits = match ifaddr.ipaddr with
        | AF_INET (a, p) ->
          (V4.to_string a),
          (V4.Prefix.bits p)
        | AF_INET6 (a, p) ->
          (V6.to_string ~v4:false a),
          (V6.Prefix.bits p) in
      Printf.printf "%s/%d\n%!" addr bits
    ) addrs
