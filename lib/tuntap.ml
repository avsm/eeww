type kind = Tap | Tun

external opentun_stub : string -> kind -> bool -> bool
  -> int -> int -> Unix.file_descr * string = "tun_opendev_byte" "tun_opendev"
external get_macaddr : string -> string = "get_macaddr"
external set_ipv4 : string -> string -> string -> unit = "set_ipv4"
external set_up_and_running : string -> unit = "set_up_and_running"

external get_ifnamsiz : unit -> int = "get_ifnamsiz"

let open_ kind ?(pi=false) ?(persist=false) ?(user = -1) ?(group = -1) ?(devname="") () =
  opentun_stub devname kind pi persist user group

let opentun = open_ Tun
let opentap = open_ Tap

(* Closing is just opening an existing device in non-persistent
   mode *)
let closetun devname = ignore (opentun ~devname ())
let closetap devname = ignore (opentap ~devname ())

let set_ipv4 ~devname ~ipv4 ?(netmask="") () = set_ipv4 devname ipv4 netmask

let get_macaddr iface = Macaddr.of_bytes_exn (get_macaddr iface)

type iface_addr_ =
    {
      addr_: int32;
      mask_: int32;
      brd_:  int32;
    }

module Ipv4 = Ipaddr.V4
type ipv4 = Ipv4.t
type iface_addr =
    {
      addr: ipv4;
      mask: ipv4;
      brd:  ipv4;
    }

type ifaddrs_ptr

external getifaddrs_stub : unit -> ifaddrs_ptr = "getifaddrs_stub"
external freeifaddrs_stub : ifaddrs_ptr -> unit = "freeifaddrs_stub"

external iface_addr : ifaddrs_ptr -> iface_addr_ option = "iface_addr"
external iface_name : ifaddrs_ptr -> string = "iface_name"
external iface_next : ifaddrs_ptr -> ifaddrs_ptr option = "iface_next"

let getifaddrs () =
  let start = getifaddrs_stub () in
  let rec loop acc ptr = match iface_next ptr with
    | None -> freeifaddrs_stub start; acc
    | Some p -> loop (match iface_addr p with
        | Some a -> (iface_name p,
                     {addr = Ipv4.of_int32 a.addr_;
                      mask = Ipv4.of_int32 a.mask_;
                      brd  = Ipv4.of_int32 a.brd_;
                     })::acc
        | None -> acc) p
  in loop [] start
