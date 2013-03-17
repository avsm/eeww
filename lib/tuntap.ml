(** Type of virtual interfaces. [Tap] refers to Layer 2 (TAP)
    interface. In this mode, [opendev] will return a file descriptor
    in which you will read and write ethernet frames. [Tun] refers to
    Layer 3 (TUN) interface, in this mode, you will read and write IP
    packets. *)
type kind = Tap | Tun

external stub_opendev : string -> kind -> bool -> Unix.file_descr = "opendev"
external stub_get_hwaddr : unit -> string = "get_hwaddr"

(** Creates or open a tuntap device. If no [devname] is specified, or
    if [devname] specify an unexistant device, new device will be
    created. Otherwise, the interface [devname] will be opened. [kind]
    allows you to choose to create (or open) an interface in TAP
    (layer2) or TUN (layer3) mode. Finally, [no_pi] is to indicate if
    you want packet information associated with your frames (TAP) or
    packets (TUN). Defaults to no information. *)
let opendev ?(pi=false) ?devname kind : Unix.file_descr =
  match devname with
  | Some n -> stub_opendev n kind pi
  | None   -> stub_opendev "" kind pi

(** [get_hwaddr devname] returns the MAC address of interface
    [devname], as a string. *)
let get_hwaddr = stub_get_hwaddr
