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

val get_type : t -> Type.t
(** [get_type t] returns the type of the endpoint *)
