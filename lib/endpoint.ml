module Type = struct
  type t = Invalid | Address | Host | Bonjour | Url
end

type t
(** The type for endpoints *)

external network_endpoint_create_host : string -> string -> t
  = "ocaml_network_endpoint_create_host"

external network_endpoint_release : t -> unit = "ocaml_network_endpoint_release"

let create_host ~hostname port =
  network_endpoint_create_host hostname (string_of_int port)

let release t = network_endpoint_release t

let get_type _ = Type.Invalid
