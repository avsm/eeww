(** [Kqueue] provides bindings to the [Kqueue] event notification interface
    available on many BSD based systems and macOS. *)

module Bindings : module type of Kqueue_stubs.Definition (Kqueue_generated_stubs)

(** [Timespec] is used to set the maximum interval a [kevent] syscall will wait
    for an event. *)
module Timespec : sig
  type t = Bindings.Timespec.t

  val sec : t -> int64

  val nsec : t -> int64

  val make : sec:int64 -> nsec:int64 -> t
end

(** [Kevent.t] is used to represent an input/output event that can be registered
    with the kernel. A kevent consists of:

    - [ident]: an identifier for an event.
    - [filter]: filter is used to determine if an event has met the
      pre-condition for it to be made available for retrieval by the user.
    - [flags]: flags are used to specify actions that need to be performed on an
      event.
    - [fflags]: filter specific flags.
    - [data]: filter specific data.
    - [udata]: User-defined data that gets passed through unchanged. *)
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

type error =
  [ `ENOMEM
  | `EMFILE
  | `ENFILE
  | `EACCES
  | `EFAULT
  | `EBADF
  | `EINTR
  | `EINVAL
  | `ENOENT
  | `ESRCH
  | `UNKNOWN of int ]

val error_message : error -> string
(** [error_message] returns a description of a kevent error code. *)

type t

val kqueue : unit -> (t, [> error]) result
(** [kqueue] creates a new kernel event queue. *)

val kqueue_exn : unit -> t

val kevent :
     ?timeout:Timespec.t
  -> t
  -> changelist:Kevent.t Ctypes_static.carray
  -> eventlist:Kevent.t Ctypes_static.carray
  -> (int, [> error]) result
(** [kevent] is used to register events with kqueue, and fetch events that are
    available to read from the kqueue. The changelist array contains the new
    events that need to be registered. The items in the changelist are processed
    before any new items are read from a kqueue. If eventlist is non empty, the
    kevent call will block indefinitely till there are n events available to
    read, n being the length of the eventlist array. The optional timeout
    parameter can be used instead to specify the maximum interval that kevent
    should wait for before returning. *)

val kevent_exn :
     ?timeout:Timespec.t
  -> t
  -> changelist:Kevent.t Ctypes_static.carray
  -> eventlist:Kevent.t Ctypes_static.carray
  -> int
