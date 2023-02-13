module Queue = Queue
module Block = Block
module Io = Io
module Group = Group
module Semaphore = Semaphore
module Data = Data
module Time = Time
module Source = Source

external dispatch_main : unit -> unit = "ocaml_dispatch_main"

external dispatch_async : Queue.t -> (unit -> unit) -> unit
  = "ocaml_dispatch_async"

external dispatch_sync : Queue.t -> (unit -> unit) -> unit
  = "ocaml_dispatch_sync"

external dispatch_after : int64 -> Queue.t -> (unit -> unit) -> unit
  = "ocaml_dispatch_after"

let async = dispatch_async
let sync = dispatch_sync
let main = dispatch_main

let after ~delay queue fn = dispatch_after delay queue fn
