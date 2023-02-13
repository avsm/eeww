type t

external dispatch_semaphore_create : int -> t = "ocaml_dispatch_semaphore_create"

external dispatch_semaphore_wait : t -> Time.t -> int
  = "ocaml_dispatch_semaphore_wait"

external dispatch_semaphore_signal : t -> unit = "ocaml_dispatch_semaphore_signal"

let create = dispatch_semaphore_create
let wait = dispatch_semaphore_wait
let signal = dispatch_semaphore_signal