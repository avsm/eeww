type t
(** The type of a bidirectional data connection between a local endpoint and a
    remote endpoint *)

val create : params:Parameters.t -> Endpoint.t -> t
(** [create ~params endpoint] initialises a new connection to a remote endpoint *)

val set_queue : queue:Dispatch.Queue.t -> t -> unit
(** [set_queue ~queue conn] sets the dispatch queue on which all events are
    delivered *)

val retain : t -> unit
(** [retain t] -- connection objects close the underlying network connection
    when its last reference is release. If you need the connection it must be
    retained *)

val start : t -> unit
(** [start conn] starts establishing a connection *)

val restart : t -> unit
(** [restart conn] restarts a connection that is in the waiting state *)

val cancel : t -> unit
(** [cancel t] cancels the connection and gracefully disconnects any established
    network protocols *)

val copy_endpoint : t -> Endpoint.t
(** [copy_endpoint conn] accesses the endpoint with which the connection was
    created *)

module State : sig
  type t =
    | Invalid
    | Waiting
    | Preparing
    | Ready
    | Failed
    | Cancelled
        (** The state indicates whether a connection can be used to send and
            receive data *)

  type handler = t -> Error.t -> unit
  (** A handler is a function which can be set to respond to state changes on a
      given connection. *)
end

val set_state_changed_handler : handler:State.handler -> t -> unit
(** Set the state changed handler for a given connection. *)

module Context : sig
  type t = Default | Final
end

type receive_completion =
  Dispatch.Data.t option -> Context.t -> bool -> Error.t -> unit

val receive : min:int -> max:int -> completion:receive_completion -> t -> unit

type send_completion = Error.t -> unit

val send :
  is_complete:bool ->
  completion:send_completion ->
  context:Context.t ->
  data:Dispatch.Data.t ->
  t ->
  unit
(** [send ~context ~is_complete ~completion ~data t] sends [data] on the
    connection [t].

    @param data The data being sent on the connection
    @param context The context associated with content *)
