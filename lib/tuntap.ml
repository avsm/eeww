type kind = Tap | Tun

external opendev_stub : string -> kind -> bool -> bool
  -> int -> int -> Unix.file_descr * string = "opendev_byte" "opendev"
external get_hwaddr : string -> string = "get_hwaddr"

let opendev ?(pi=false) ?(persist=false) ?(user = -1) ?(group = -1) ?(devname="") kind =
  opendev_stub devname kind pi persist user group

let string_of_hwaddr mac =
  let ret = ref "" in
  let _ =
    String.iter (
      fun ch ->
        ret := Printf.sprintf "%s%02x:" !ret (int_of_char ch)
    ) mac  in
  String.sub !ret 0 (String.length !ret - 1)
