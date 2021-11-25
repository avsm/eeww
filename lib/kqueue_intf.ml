module type S = sig
  type t

  module Timeout : sig
    (** [Kqueue.Timeout.t] is the timespan in nanoseconds that is used to represent the
        maximum amount of time the kevent call should wait for an event. *)
    type t

    (** [never] results in the kevent call to wait indefinitely for a new event.

        {e Note:} Unlike [Unix.select], if the user calls kevent with an empty eventlist,
        the kevent call returns immediately event when a timeout value of [never] is used. *)
    val never : t

    (** [immediate] effects a poll, and the kevent call will return immediately, without
        waiting for new events if none are available at the time of the call. *)
    val immediate : t

    (** [of_ns] creates a new timeout from nanoseconds. *)
    val of_ns : int64 -> t
  end

  module Note : sig
    (** [Kqueue.Note.t] represents the kqueue filter specific flags. These are used in
        combination with the data field to augment the behavior of kqueue event filters.

        Consult the kqueue manpages to see what the various Note values represent.

        - {{:https://www.freebsd.org/cgi/man.cgi?kqueue} FreeBSD}
        - {{:https://man.openbsd.org/kqueue.2} OpenBSD}*)
    type t

    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
    val ( = ) : t -> t -> bool
    val empty : t
    val seconds : t
    val useconds : t
    val nseconds : t
    val lowat : t
    val oob : t
    val delete : t
    val write : t
    val extend : t
    val attrib : t
    val link : t
    val rename : t
    val revoke : t
    val exit : t
    val fork : t
    val exec : t
    val signal : t
  end

  module Filter : sig
    (** [Kqueue.Filter.t] represents the kernel filter used to process an event. *)
    type t

    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
    val ( = ) : t -> t -> bool
    val read : t
    val write : t
    val timer : t
    val vnode : t
    val proc : t
  end

  module Flag : sig
    (** [Kqueue.Flag.t] is a set of flags that are used to indicate which actions should
        be performed for an event. *)
    type t

    val equal : t -> t -> bool
    val ( = ) : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val ( + ) : t -> t -> t
    val intersect : t -> t -> bool
    val receipt : t
    val add : t
    val enable : t
    val disable : t
    val delete : t
    val oneshot : t
    val clear : t
    val eof : t
    val error : t
  end

  module Event_list : sig
    (** [Kqueue.Event_list.t] represents a list of kevents that can be used to populate
        either the changelist or the eventlist for the kevent syscall. The intended usage
        is to allocate a list of events using [create], and then retrieve events at a
        particular index using [get idx], and then using the [Kqueue.Event_list.Event]
        module to read/write the varios fields that belong to a kevent. *)
    type t

    val null : t
    val create : int -> t

    module Event : sig
      type t

      val get_ident : t -> int
      val set_ident : t -> int -> unit
      val get_filter : t -> Filter.t
      val set_filter : t -> Filter.t -> unit
      val get_flags : t -> Flag.t
      val set_flags : t -> Flag.t -> unit
      val get_fflags : t -> Note.t
      val set_fflags : t -> Note.t -> unit
      val get_data : t -> int
      val set_data : t -> int -> unit
      val get_udata : t -> int
      val set_udata : t -> int -> unit
    end

    val get : t -> int -> Event.t
  end

  val create : unit -> t
  val kevent : t -> changelist:Event_list.t -> eventlist:Event_list.t -> Timeout.t -> int
  val close : t -> unit

  module Util : sig
    (** [file_descr_to_int] will convert a [Unix.file_descr] to an integer. This is
        intended to be used to create an kevent ident when registering a file descriptor
        based kevent. *)
    val file_descr_to_int : Unix.file_descr -> int

    (** [file_descr_to_int] will convert an integer to a [Unix.file_descr]. *)
    val file_descr_of_int : int -> Unix.file_descr
  end

  (** [available] Indicates if the system where this library was built has kqueue
      available. *)
  val available : bool
end
