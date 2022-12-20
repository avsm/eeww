module Type : sig
  type t =
    | Invalid
    | Address
    | Host
    | Bonjour
    | Url  (** Different endpoint types *)
end

type t
(** The type for endpoints *)

val create_host : hostname:string -> int -> t
(** [create_host ~hostname port] will create a network endpoint with a
    [hostname] and a [port], the [hostname] can be interpretted as an IP address *)

val create_address : Unix.sockaddr -> t
(** [create_address sock] will create a network endpoint with a [Unix.sockaddr] *)

val release : t -> unit

val get_type : t -> Type.t
(** [get_type t] returns the type of the endpoint *)

val get_address : t -> Unix.sockaddr option
(** [get_address t] returns the socket address of the endpoint if [get_type t]
    is {!Type.Address}, otherwise [None] is returned. *)
