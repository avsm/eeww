open Import
module Timespec = Timespec
module Flag = Flag

type t = int

let kqueue () = Bindings.kqueue ()
