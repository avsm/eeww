module Timer = struct
  type t

  external dispatch_source_timer_create : Queue.t -> t = "ocaml_dispatch_source_timer_create"
  external dispatch_source_timer_set_event_handler : t -> (unit -> unit) -> unit = "ocaml_dispatch_source_timer_set_event_handler"

  external dispatch_source_timer_start : t -> int64 -> unit = "ocaml_dispatch_source_timer_start"

  external dispatch_source_timer_stop : t -> unit = "ocaml_dispatch_source_timer_stop"

  let create queue = dispatch_source_timer_create queue

  let set_event_handler t e = dispatch_source_timer_set_event_handler t e

  let start t = dispatch_source_timer_start t
  let stop t = dispatch_source_timer_stop t
end