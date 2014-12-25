open Tuntap
open Ipaddr

let () =
  let addrs = getifaddrs () in
  List.iter (fun (name, ip) ->
      Printf.printf "%s -> " name;
      let addr, bits = match ip with
        | `V4 (a, p) ->
          (V4.to_string a),
          (V4.Prefix.bits p)
        | `V6 (a, p) ->
          (V6.to_string ~v4:false a),
          (V6.Prefix.bits p) in
      Printf.printf "%s/%d\n%!" addr bits
    ) addrs
