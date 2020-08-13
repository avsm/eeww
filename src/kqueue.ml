module Bindings = Kqueue_stubs.Definition (Kqueue_generated_stubs)

type t = int

let kqueue () = Bindings.kqueue ()
