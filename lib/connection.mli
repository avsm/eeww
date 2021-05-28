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

val cancel : t -> unit
(** [cancel t] cancels the connection and gracefully disconnects any established
    network protocols *)

module State : sig
  type t = Invalid | Waiting | Preparing | Ready | Failed | Cancelled

  type handler = t -> Error.t -> unit
end

val set_state_changed_handler : handler:State.handler -> t -> unit

module Context : sig
  type t

  val default : unit -> t

  val retain : t -> unit

  val release : t -> unit
end

type receive_completion =
  Dispatch.Data.t -> Context.t -> bool -> Error.t -> unit

val receive : min:int -> max:int -> completion:receive_completion -> t -> unit

type send_completion = Error.t -> unit

val send :
  is_complete:bool ->
  completion:send_completion ->
  context:Context.t ->
  data:Dispatch.Data.t ->
  t ->
  unit
(** [send ~context ~is_complete ~completion ~content t] sends [data] on the
    connection [t].

    @param content The data being sent on the connection
    @param context The context associated with content *)
