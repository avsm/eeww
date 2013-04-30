(** Module for dealing with TUN/TAP devices. TUN refers to layer 3
    virtual interfaces whereas TAP refers to layer 2 ones. *)


(** [opentun ~pi ~persist ~user ~group ~devname ()] will create a TUN
    interface. If [devname] is specified, or if [devname] specify an
    unexistant device, a new device will be created, otherwise, the
    interface [devname] will be opened. [no_pi] is to indicate if you
    want packet information associated with your frames (tap) or
    packets (tun) (defaults to no information). [persist] will set the
    device persistent with permissions set to [user] and [group] if
    supported by your OS (currently MacOSX does not support it). The
    return value is a pair consisting of a fd opened on the freshly
    created interface, and its name as will be displayed by command
    [ifconfig] for example. *)
val opentun : ?pi:bool -> ?persist:bool -> ?user:int
  -> ?group:int -> ?devname:string -> unit -> Unix.file_descr * string

(** Like [opentun], but open TAP interfaces instead of TUN ones. *)
val opentap : ?pi:bool -> ?persist:bool -> ?user:int
  -> ?group:int -> ?devname:string -> unit -> Unix.file_descr * string

(** [closetun devname kind] will destroy [devname], if it exists. *)
val closetun : string -> unit

(** Like [closetun], but for TAP interfaces. *)
val closetap : string -> unit

(** [get_ifnamsiz ()] returns the value of the constant IFNAMSIZ,
		defined in <net/if.h>. Useful for allocating buffers that contain
		interface names. *)
val get_ifnamsiz : unit -> int

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

(** Generate a locally administered unicast MAC address *)
val make_local_hwaddr : unit -> string
