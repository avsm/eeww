type t

external network_listener_create : Parameters.t -> t
  = "ocaml_network_listener_create"

external network_listener_create_with_port : string -> Parameters.t -> t
  = "ocaml_network_listener_create_with_port"

external network_listener_create_with_connection :
  Connection.t -> Parameters.t -> t
  = "ocaml_network_listener_create_with_connection"

let create params = network_listener_create params

let create_with_port ~port params =
  network_listener_create_with_port (string_of_int port) params

let create_with_connection ~connection params =
  network_listener_create_with_connection connection params

let get_port _ = 1

let set_queue ~queue:_ _ = ()

let start _ = ()

let cancel _ = ()
