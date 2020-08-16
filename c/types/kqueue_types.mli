module Definition (T : Cstubs.Types.TYPE) : sig
  include module type of Kqueue_constants.Definition (Kqueue_generated_constants)

  module Timespec : sig
    type t = [`Timespec] Ctypes.structure

    val t : t T.typ

    val tv_sec : (PosixTypes.time_t, t) T.field

    val tv_nsec : (Signed.long, t) T.field
  end

  module Kevent : sig
    type t = [`Kevent] Ctypes.structure

    val t : t T.typ

    val ident : (T.Uintptr.t, t) T.field

    val filter : (int, t) T.field

    val flags : (Unsigned.UInt16.t, t) T.field

    val fflags : (Unsigned.UInt32.t, t) T.field

    val data : (T.Intptr.t, t) T.field

    val udata : (T.Uintptr.t, t) T.field
  end
end
