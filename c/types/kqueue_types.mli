open Ctypes

module Definition (T : Cstubs.Types.TYPE) : sig
  module Timespec : sig
    type t = [`Timespec] structure

    val t : t T.typ

    val tv_sec : (PosixTypes.time_t, t) T.field

    val tv_nsec : (Signed.long, t) T.field
  end

  module Kevent : sig
    type t = [`Kevent] structure

    val t : t T.typ

    val ident : (Unix.file_descr, t) T.field

    val filter : (int, t) T.field

    val flags : (Unsigned.UInt16.t, t) T.field

    val fflags : (Unsigned.UInt32.t, t) T.field

    val data : (T.Intptr.t, t) T.field

    val udata : (T.Uintptr.t, t) T.field
  end

  module Flags : sig
    val ev_add : Unsigned.uint16 T.const

    val ev_enable : Unsigned.uint16 T.const

    val ev_disable : Unsigned.uint16 T.const

    val ev_dispatch : Unsigned.uint16 T.const

    val ev_delete : Unsigned.uint16 T.const

    val ev_receipt : Unsigned.uint16 T.const

    val ev_oneshot : Unsigned.uint16 T.const

    val ev_clear : Unsigned.uint16 T.const

    val ev_eof : Unsigned.uint16 T.const

    val ev_error : Unsigned.uint16 T.const
  end

  module Filters : sig
    val evfilt_read : int T.const

    val evfilt_write : int T.const

    val evfilt_aio : int T.const

    val evfilt_vnode : int T.const

    val evfilt_proc : int T.const

    val evfilt_signal : int T.const

    val evfilt_timer : int T.const

    val evfilt_user : int T.const

    val evfilt_fs : int T.const
  end
end
