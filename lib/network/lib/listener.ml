type t

external network_listener_create : Parameters.t -> t
  = "ocaml_network_listener_create"

external network_listener_create_with_port : string -> Parameters.t -> t
  = "ocaml_network_listener_create_with_port"

external network_listener_create_with_connection :
  Connection.t -> Parameters.t -> t
  = "ocaml_network_listener_create_with_connection"

external network_listener_get_port : t -> int
  = "ocaml_network_listener_get_port"

external network_listener_set_queue : Dispatch.Queue.t -> t -> unit
  = "ocaml_network_listener_set_queue"

external network_listener_cancel : t -> unit = "ocaml_network_listener_cancel"

external network_listener_start : t -> unit = "ocaml_network_listener_start"

external network_listener_retain : t -> unit = "ocaml_network_listener_retain"

external network_listener_release : t -> unit = "ocaml_network_listener_release"

let create params = network_listener_create params

let create_with_port ~port params =
  network_listener_create_with_port (string_of_int port) params

let create_with_connection ~connection params =
  network_listener_create_with_connection connection params

let get_port t = network_listener_get_port t

let set_queue ~queue t = network_listener_set_queue queue t

let start t = network_listener_start t

let cancel = network_listener_cancel

let retain = network_listener_retain

let release = network_listener_release

module State = struct
  type t = Invalid | Waiting | Ready | Failed | Cancelled

  type handler = t -> Error.t -> unit
end

external network_listener_set_state_changed_handler : State.handler -> t -> unit
  = "ocaml_network_listener_set_state_changed_handler"

let set_state_changed_handler ~handler t =
  network_listener_set_state_changed_handler handler t

external network_listener_set_new_connection_handler :
  (Connection.t -> unit) -> t -> unit
  = "ocaml_network_listener_set_new_connection_handler"

let set_new_connection_handler ~handler t =
  network_listener_set_new_connection_handler handler t
