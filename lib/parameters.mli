type t
(** Initial parameters for TLS or TCP connections and listeners *)

val create : unit -> t
(** [create ()] initialises parameters for connections with no protocol
    specified *)

val create_tcp : unit -> t
(** [create_tcp ()] initialises parameters for connections using tcp without tls *)

val set_local_endpoint : endpoint:Endpoint.t -> t -> unit
(** [set_local_endpoint ~endpoint t] sets a specifc local IP address and port to
    use for connections *)

val set_reuse_local_address : t -> bool -> unit
(** [set_reuse_local_address] allows reusing local addresses and ports across
    connections. Similar to the [SO_REUSEADDR] socket option. *)
