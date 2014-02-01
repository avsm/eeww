open Tuntap

let unopt = function
  | Some v -> v
  | None -> None

let () =
  let addrs = getifaddrs () in
  List.iter (fun ifaddr ->
      match ifaddr.addr with
      | Some a ->
        Printf.printf "%s -> %s\n%!"
          ifaddr.name
          (Ipaddr.to_string a)
      | None -> ()
    ) addrs
