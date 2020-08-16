module Definition (T : Cstubs.Types.TYPE) = struct
  open T
  include Kqueue_constants.Definition (Kqueue_generated_constants)

  module Timespec = struct
    type t = [`Timespec] Ctypes.structure

    let t : t typ = structure "timespec"

    let tv_sec = field t "tv_sec" (lift_typ PosixTypes.time_t)

    let tv_nsec = field t "tv_nsec" long

    let () = seal t
  end

  module Kevent = struct
    type t = [`Kevent] Ctypes.structure

    let t : t typ = structure "kevent"

    let ident = field t "ident" uintptr_t

    let filter = field t "filter" short

    let flags = field t "flags" uint16_t

    let fflags = field t "fflags" uint32_t

    let data = field t "data" intptr_t

    let udata = field t "udata" uintptr_t

    let () = seal t
  end
end
