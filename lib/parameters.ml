type t

external network_parameters_create : unit -> t
  = "ocaml_network_parameters_create"

external network_parameters_create_tcp : unit -> t
  = "ocaml_network_parameters_create_tcp"

external network_parameters_create_udp : unit -> t
  = "ocaml_network_parameters_create_udp"

external network_parameters_set_local_endpoint : t -> Endpoint.t -> unit
  = "ocaml_network_parameters_set_local_endpoint"

external network_parameters_set_reuse_local_address : t -> bool -> unit
  = "ocaml_network_parameters_set_reuse_local_address"

external network_parameters_set_allow_fast_open : t -> bool -> unit
  = "ocaml_network_parameters_set_allow_fast_open"

let create = network_parameters_create

let create_tcp = network_parameters_create_tcp

let create_udp = network_parameters_create_udp

let set_local_endpoint ~endpoint t =
  network_parameters_set_local_endpoint t endpoint

let set_reuse_local_address t b = network_parameters_set_reuse_local_address t b

let set_allow_fast_open t b = network_parameters_set_allow_fast_open t b
