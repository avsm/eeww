type t
(** Initial parameters for TLS or TCP connections and listeners *)

val create : unit -> t
(** [create ()] initialises parameters for connections with no protocol
    specified *)
