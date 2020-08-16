module Definition (F : Cstubs.FOREIGN) = struct
  module Types = Kqueue_types.Definition (Kqueue_generated_types)
  include Types

  module Ctypes = struct
    include Ctypes

    let ( @-> ) = F.( @-> )

    let returning = F.returning

    let foreign = F.foreign
  end

  open Ctypes

  let kqueue = F.foreign "kqueue" (void @-> returning int)

  let kevent =
    foreign "kevent"
      ( int @-> ptr Kevent.t @-> int @-> ptr Kevent.t @-> int @-> ptr Timespec.t
      @-> returning int )
end
