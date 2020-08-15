open Import
module Timespec = Timespec
module Flag = Flag
module Kevent = Kevent
module Changes = Changes

type t = int

let kqueue () = Bindings.kqueue ()
