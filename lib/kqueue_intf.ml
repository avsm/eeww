module type S = sig
  type t

  module Timeout : sig
    type t

    val never : t
    val immediate : t
    val of_ns : int64 -> t
  end

  module Note : sig
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
    val file_descr_to_int : Unix.file_descr -> int
    val file_descr_of_int : int -> Unix.file_descr
  end
end
