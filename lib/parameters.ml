type t = unit

external network_parameters_create : unit -> t
  = "ocaml_network_parameters_create"

(* external network_parameters_create_secure_tcp : unit -> t
  = "ocaml_network_parameters_create" *)

let create = network_parameters_create
