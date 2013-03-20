(** Type of virtual interfaces. [Tap] refers to Layer 2 (TAP)
    interface. In this mode, [opendev] will return a file descriptor
    in which you will read and write ethernet frames. [Tun] refers to
    Layer 3 (TUN) interface, in this mode, you will read and write IP
    packets. *)
type kind = Tap | Tun

(** Creates or open a tuntap device. If no [devname] is specified, or
    if [devname] specify an unexistant device, new device will be
    created. Otherwise, the interface [devname] will be opened. [kind]
    allows you to choose to create (or open) an interface in TAP
    (layer2) or TUN (layer3) mode. [no_pi] is to indicate if you want
    packet information associated with your frames (TAP) or packets
    (TUN) (defaults to no information). [persist] will set the device
    persistent with permissions set to [user] and [group] if supported
    by your OS (currently MacOSX does not support it). *)
val tun_opendev : ?pi:bool -> ?persist:bool -> ?user:int
  -> ?group:int -> ?devname:string -> kind -> Unix.file_descr * string

(** [get_hwaddr devname] returns the MAC address of interface
    [devname], as a raw string (not hexa). *)
val get_hwaddr : string -> string

(** [set_ipv4 devname ipv4addr netmask] assign an IPv4 to interface
    [devname] *)
val set_ipv4 : ?netmask:string -> string -> string -> unit

(** [set_up_and_running devname] sets interface [devname] up and
    running. *)
val set_up_and_running : string -> unit

(** [string_of_hwaddr hwaddr] returns the MAC address in the (usual)
    hex format *)
val string_of_hwaddr : string -> string
