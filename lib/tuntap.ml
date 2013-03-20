type kind = Tap | Tun

external tun_opendev : string -> kind -> bool -> bool
  -> int -> int -> Unix.file_descr * string = "tun_opendev_byte" "tun_opendev"
external get_hwaddr : string -> string = "get_hwaddr"
external set_ipv4 : string -> string -> string -> unit = "set_ipv4"
external set_up_and_running : string -> unit = "set_up_and_running"

let tun_opendev ?(pi=false) ?(persist=false) ?(user = -1) ?(group = -1) ?(devname="") kind =
  tun_opendev devname kind pi persist user group

let set_ipv4 ?(netmask="") dev ipv4addr = set_ipv4 dev ipv4addr netmask

let string_of_hwaddr mac =
  let ret = ref "" in
  let _ =
    String.iter (
      fun ch ->
        ret := Printf.sprintf "%s%02x:" !ret (int_of_char ch)
    ) mac  in
  String.sub !ret 0 (String.length !ret - 1)
