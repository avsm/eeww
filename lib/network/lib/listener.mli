type t
(** The type for listeners of incoming network connections *)

val create : Parameters.t -> t
(** [create params] creates a new listener using the supplied parameters *)

val create_with_port : port:int -> Parameters.t -> t

val create_with_connection : connection:Connection.t -> Parameters.t -> t

val get_port : t -> int
(** [get_port t] is the port [t] is listening on *)

val set_queue : queue:Dispatch.Queue.t -> t -> unit
(** [set_queue ~queue t] sets the queue on which all listener events are
    delivered *)

val start : t -> unit
(** [start t] registers [t] for listening for inbound connections *)

val cancel : t -> unit
(** [cancel t] stopos [t] listening for inbound connections *)

val retain : t -> unit

val release : t -> unit

module State : sig
  type t = Invalid | Waiting | Ready | Failed | Cancelled

  type handler = t -> Error.t -> unit
end

val set_state_changed_handler : handler:State.handler -> t -> unit

val set_new_connection_handler : handler:(Connection.t -> unit) -> t -> unit
