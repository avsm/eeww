type t
(** The type of a bidirectional data connection between a local endpoint and a
    remote endpoint *)

(* val create : Endpoint.t -> Parameters.t -> t
(** [create endpoint params] initialises a new connection to a remote endpoint *)

val set_queue : queue:Dispatch.Queue.t -> t -> unit
(** [set_queue ~queue conn] sets the dispatch queue on which all events are
    delivered *)

val start : t -> unit
(** [start conn] starts establishing a connection *)

val restart : t -> unit
(** [restart t] restarts a connection in a waiting state *)

type send_completion = Error.t -> unit

type context

val send :
  context:context ->
  is_complete:bool ->
  completion:send_completion ->
  content:Dispatch.Data.t ->
  t ->
  unit
(** [send ~context ~is_complete ~completion ~content t] sends [data] on the
    connection [t].

    @param content The data being sent on the connection
    @param context The context associated with content *)

type receive_completion = Dispatch.Data.t -> context -> bool -> Error.t -> unit

val receive : min:int32 -> max:int32 -> completion:t -> unit

val get_max_datagram_size : t -> int
(** Accesses the maximum datagram size of the connection *)

module State : sig
  type t = Invalid | Waiting | Preparing | Ready | Failed | Cancelled

  type handler = t -> Error.t -> unit
end

val set_state_changed_handler : handler:State.handler -> t -> unit *)
