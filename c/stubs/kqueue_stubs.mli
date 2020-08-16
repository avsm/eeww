module Definition (F : Cstubs.FOREIGN) : sig
  include module type of Kqueue_types.Definition (Kqueue_generated_types)

  val kqueue : (unit -> int F.return) F.result

  val kevent :
    (   int
     -> Kevent.t Ctypes_static.ptr
     -> int
     -> Kevent.t Ctypes_static.ptr
     -> int
     -> Timespec.t Ctypes_static.ptr
     -> int F.return)
    F.result
end
