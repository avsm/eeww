module Type = struct
  type t =
    | Invalid
    | Address
    | Host
    | Bonjour
    | Url  (** Types of network endpoints *)
end

type t
(** A local or remote endpoint in a network connection *)

external network_endpoint_create_host : string -> string -> t
  = "ocaml_network_endpoint_create_host"

external network_endpoint_get_type : t -> Type.t
  = "ocaml_network_endpoint_get_type"

external network_endpoint_release : t -> unit = "ocaml_network_endpoint_release"

external network_endpoint_create_address : Unix.sockaddr -> t
  = "ocaml_network_endpoint_create_address"

external network_endpoint_get_address : t -> Unix.sockaddr
  = "ocaml_network_endpoint_get_address"

let create_host ~hostname port =
  network_endpoint_create_host hostname (string_of_int port)

let create_address sock = network_endpoint_create_address sock

let release t = network_endpoint_release t

let get_type = network_endpoint_get_type

let get_address t =
  match get_type t with
  | Address -> Some (network_endpoint_get_address t)
  | _ -> None
