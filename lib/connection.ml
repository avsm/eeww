open Dispatch

type t

external network_connection_create : Endpoint.t -> Parameters.t -> t
  = "ocaml_network_connection_create"

external network_connection_set_queue : Queue.t -> t -> unit
  = "ocaml_network_connection_set_queue"

external network_connection_retain : t -> unit
  = "ocaml_network_connection_retain"

external network_connection_start : t -> unit = "ocaml_network_connection_start"

external network_connection_restart : t -> unit
  = "ocaml_network_connection_restart"

external network_connection_cancel : t -> unit
  = "ocaml_network_connection_cancel"

external network_connection_copy_endpoint : t -> Endpoint.t
  = "ocaml_network_connection_copy_endpoint"

let create ~params endpoint = network_connection_create endpoint params

let set_queue ~queue t = network_connection_set_queue queue t

let retain t = network_connection_retain t

let start t = network_connection_start t

let restart t = network_connection_restart t

let cancel t = network_connection_cancel t

let copy_endpoint t = network_connection_copy_endpoint t

module State = struct
  type t = Invalid | Waiting | Preparing | Ready | Failed | Cancelled

  type handler = t -> Error.t -> unit
end

external network_connection_set_state_changed_handler :
  State.handler -> t -> unit
  = "ocaml_network_connection_set_state_changed_handler"

let set_state_changed_handler ~handler t =
  network_connection_set_state_changed_handler handler t

module Context = struct
  type t = Default | Final
end

type receive_completion =
  Dispatch.Data.t option -> Context.t -> bool -> Error.t -> unit

external network_connection_receive :
  int -> int -> receive_completion -> t -> unit
  = "ocaml_network_connection_receive"

let receive ~min ~max ~completion t =
  network_connection_receive min max completion t

type send_completion = Error.t -> unit

external network_connection_send :
  Dispatch.Data.t -> Context.t -> bool -> send_completion -> t -> unit
  = "ocaml_network_connection_send"

let send ~is_complete ~completion ~context ~data t =
  network_connection_send data context is_complete completion t
