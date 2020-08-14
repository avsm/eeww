module Definition (F : Cstubs.FOREIGN) = struct
  module Ctypes = struct
    include Ctypes

    let ( @-> ) = F.( @-> )

    let returning = F.returning

    let foreign = F.foreign

    let foreign_value = F.foreign_value
  end

  open Ctypes

  module Timespec = struct
    type t = [`Timespec] structure

    let t : t typ = structure "timespec"

    let tv_sec = field t "tv_sec" (lift_typ PosixTypes.time_t)

    let tv_nsec = field t "tv_nsec" long

    let () = seal t
  end

  module Kevent = struct
    type t = [`Kevent] structure

    let t : t typ = structure "kevent"

    let ident = field t "ident" int

    let filter = field t "filter" short

    let flags = field t "flags" uint16_t

    let fflags = field t "fflags" uint32_t

    let data = field t "data" intptr_t

    let udata = field t "udata" uintptr_t

    let () = seal t
  end

  let kqueue = foreign "kqueue" (void @-> returning int)

  let kevent =
    foreign "kevent"
      ( int @-> ptr Kevent.t @-> int @-> ptr Kevent.t @-> int @-> ptr Timespec.t
      @-> returning int )
end
