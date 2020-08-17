module Bindings : module type of Kqueue_stubs.Definition (Kqueue_generated_stubs)

module Timespec : sig
  type t = Bindings.Timespec.t

  val sec : t -> int64

  val nsec : t -> int64

  val make : sec:int64 -> nsec:int64 -> t
end

module Kevent : sig
  type t = Bindings.Kevent.t

  val make :
       ident:Ctypes.Uintptr.t
    -> filter:int
    -> flags:Unsigned.uint16
    -> fflags:Unsigned.uint32
    -> data:Ctypes.Intptr.t
    -> udata:Ctypes.Uintptr.t
    -> t

  val ident : t -> Ctypes.Uintptr.t

  val filter : t -> int

  val flags : t -> Unsigned.uint16

  val fflags : t -> Unsigned.uint32

  val data : t -> Ctypes.Intptr.t

  val udata : t -> Ctypes.Uintptr.t
end

type t

val to_int : t -> int

val kqueue : unit -> t

val kevent :
     ?timeout:Timespec.t
  -> t
  -> changelist:Kevent.t Ctypes_static.carray
  -> eventlist:Kevent.t Ctypes_static.carray
  -> int
