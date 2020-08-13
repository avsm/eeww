module Definition (F : Cstubs.FOREIGN) = struct
  module Ctypes = struct
    include Ctypes

    let ( @-> ) = F.( @-> )

    let returning = F.returning

    let foreign = F.foreign

    let foreign_value = F.foreign_value
  end

  open Ctypes
  module Types = Kqueue_types.Definition (Kqueue_generated_types)
  module Kevent = Types.Kevent
  module Timespec = Types.Timespec
  module Flags = Types.Flags
  module Filters = Types.Filters

  let kqueue = foreign "kqueue" (void @-> returning int)

  let kevent =
    foreign "kevent"
      ( int @-> ptr Kevent.t @-> int @-> ptr Kevent.t @-> int @-> ptr Timespec.t
      @-> returning int )
end
