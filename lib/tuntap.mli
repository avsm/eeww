(** Creates or open a tun or tap device. tun refers to layer3 virtual
    interfaces whereas tap refers to layer2 virtual interfaces. If no
    [devname] is specified, or if [devname] specify an unexistant
    device, a new device will be created. Otherwise, the interface
    [devname] will be opened. [no_pi] is to indicate if you want
    packet information associated with your frames (tap) or packets
    (tun) (defaults to no information). [persist] will set the device
    persistent with permissions set to [user] and [group] if supported
    by your OS (currently MacOSX does not support it). *)
val opentun : ?pi:bool -> ?persist:bool -> ?user:int
  -> ?group:int -> ?devname:string -> unit -> Unix.file_descr * string
val opentap : ?pi:bool -> ?persist:bool -> ?user:int
  -> ?group:int -> ?devname:string -> unit -> Unix.file_descr * string

(** [closetun devname kind] will destroy [devname], if it exists. *)
val closetun : string -> unit
val closetap : string -> unit

(** [get_hwaddr devname] returns the MAC address of interface
    [devname], as a raw string (not hexa). *)
val get_hwaddr : string -> string

(** [set_ipv4 devname ipv4addr netmask] assign an IPv4 to interface
    [devname] *)
val set_ipv4 : dev:string -> ipv4:string -> ?netmask:string -> unit -> unit

(** [set_up_and_running devname] sets interface [devname] up and
    running. Note that when using the [set_ipv4] function, the
    interface will automatically be set up and running *)
val set_up_and_running : string -> unit

(** [string_of_hwaddr hwaddr] returns the MAC address in the (usual)
    hex format *)
val string_of_hwaddr : string -> string
