[%%import "config.h"]

module Null = struct
  type t

  let available = false

  module Timeout = struct
    type t = [ `Not_implemented ]

    let never = `Not_implemented
    let immediate = `Not_implemented
    let of_ns _ = assert false
  end

  module Util = struct
    let file_descr_to_int : Unix.file_descr -> int = fun _ -> assert false
    let file_descr_of_int : int -> Unix.file_descr = fun _ -> assert false
  end

  module Note = struct
    type t = [ `Not_implemented ]

    let equal _ _ = assert false
    let ( = ) = equal
    let seconds = `Not_implemented
    let empty = `Not_implemented
    let useconds = `Not_implemented
    let nseconds = `Not_implemented
    let lowat = `Not_implemented
    let oob = `Not_implemented
    let delete = `Not_implemented
    let write = `Not_implemented
    let extend = `Not_implemented
    let attrib = `Not_implemented
    let link = `Not_implemented
    let rename = `Not_implemented
    let revoke = `Not_implemented
    let exit = `Not_implemented
    let fork = `Not_implemented
    let exec = `Not_implemented
    let signal = `Not_implemented

    [%%if defined EVFILT_USER_AVAILABLE]

    let ffnop = `Not_implemented
    let ffand = `Not_implemented
    let ffor = `Not_implemented
    let ffcopy = `Not_implemented
    let ffctrlmask = `Not_implemented
    let fflagsmask = `Not_implemented
    let trigger = `Not_implemented

    [%%endif]
  end

  module Filter = struct
    type t = [ `Not_implemented ]

    let pp _ _ = assert false
    let equal _ _ = assert false
    let ( = ) = equal
    let read = `Not_implemented
    let write = `Not_implemented
    let timer = `Not_implemented
    let vnode = `Not_implemented
    let proc = `Not_implemented

    [%%if defined EVFILT_USER_AVAILABLE]

    let user = `Not_implemented

    [%%endif]
  end

  module Flag = struct
    type t = [ `Not_implemented ]

    let pp _ _ = assert false
    let equal _ _ = assert false
    let ( = ) = equal
    let ( + ) _ _ = assert false
    let intersect _ _ = assert false
    let receipt = `Not_implemented
    let add = `Not_implemented
    let enable = `Not_implemented
    let disable = `Not_implemented
    let delete = `Not_implemented
    let oneshot = `Not_implemented
    let clear = `Not_implemented
    let eof = `Not_implemented
    let error = `Not_implemented
  end

  module Event_list = struct
    type t = [ `Not_implemented ]

    let null = `Not_implemented
    let create _ = assert false

    module Event = struct
      type t = [ `Not_implemented ]

      let get_ident _ = assert false
      let set_ident _ _ = assert false
      let get_filter _ = assert false
      let set_filter _ _ = assert false
      let get_flags _ = assert false
      let set_flags _ _ = assert false
      let get_fflags _ = assert false
      let set_fflags _ _ = assert false
      let get_data _ = assert false
      let set_data _ _ = assert false
      let get_udata _ = assert false
      let set_udata _ _ = assert false
    end

    let get _ _ = assert false
  end

  let create () = assert false
  let kevent _ ~changelist:_ ~eventlist:_ _ = assert false
  let close _ = assert false
end

module _ : Kqueue_intf.S = struct
  include Null
end

[%%if defined KQUEUE_AVAILABLE && defined KQUEUE_ML_ARCH_SIXTYFOUR]

module Util = struct
  let file_descr_to_int : Unix.file_descr -> int = Obj.magic
  let file_descr_of_int : int -> Unix.file_descr = Obj.magic
end

module Ffi = struct
  external kqueue : unit -> Unix.file_descr = "kqueue_ml_kqueue_create"

  external kevent
    :  Unix.file_descr
    -> Bigstring.t
    -> Bigstring.t
    -> int64
    -> int
    = "kqueue_ml_kevent"
end

module Note = struct
  type t = int

  let equal = Int.equal
  let ( = ) = equal
  let empty = 0

  external seconds : unit -> int = "kqueue_note_seconds"

  let seconds = seconds ()

  external useconds : unit -> int = "kqueue_note_useconds"

  let useconds = useconds ()

  external nseconds : unit -> int = "kqueue_note_nseconds"

  let nseconds = nseconds ()

  external lowat : unit -> int = "kqueue_note_lowat"

  let lowat = lowat ()

  external oob : unit -> int = "kqueue_note_oob"

  let oob = oob ()

  external delete : unit -> int = "kqueue_note_delete"

  let delete = delete ()

  external write : unit -> int = "kqueue_note_write"

  let write = write ()

  external extend : unit -> int = "kqueue_note_extend"

  let extend = extend ()

  external attrib : unit -> int = "kqueue_note_attrib"

  let attrib = attrib ()

  external link : unit -> int = "kqueue_note_link"

  let link = link ()

  external rename : unit -> int = "kqueue_note_rename"

  let rename = rename ()

  external revoke : unit -> int = "kqueue_note_revoke"

  let revoke = revoke ()

  external exit : unit -> int = "kqueue_note_exit"

  let exit = exit ()

  external fork : unit -> int = "kqueue_note_fork"

  let fork = fork ()

  external exec : unit -> int = "kqueue_note_exec"

  let exec = exec ()

  external signal : unit -> int = "kqueue_note_signal"

  let signal = signal ()

  [%%if defined EVFILT_USER_AVAILABLE]

  external ffnop : unit -> int = "kqueue_note_ffnop"

  let ffnop = ffnop ()

  external ffand : unit -> int = "kqueue_note_ffand"

  let ffand = ffand ()

  external ffor : unit -> int = "kqueue_note_ffor"

  let ffor = ffor ()

  external ffcopy : unit -> int = "kqueue_note_ffcopy"

  let ffcopy = ffcopy ()

  external ffctrlmask : unit -> int = "kqueue_note_ffctrlmask"

  let ffctrlmask = ffctrlmask ()

  external fflagsmask : unit -> int = "kqueue_note_fflagsmask"

  let fflagsmask = fflagsmask ()

  external trigger : unit -> int = "kqueue_note_trigger"

  let trigger = trigger ()

  [%%endif]
end

module Flag = struct
  type t = int

  let equal = Int.equal
  let ( = ) = equal
  let ( + ) = ( lor )
  let intersect t1 t2 = t1 land t2 <> 0
  let is_subset t ~of_:flags = t = t land flags

  external add : unit -> int = "kqueue_flag_ev_add"

  let add = add ()

  external receipt : unit -> int = "kqueue_flag_ev_receipt"

  let receipt = receipt ()

  external enable : unit -> int = "kqueue_flag_ev_enable"

  let enable = enable ()

  external disable : unit -> int = "kqueue_flag_ev_disable"

  let disable = disable ()

  external delete : unit -> int = "kqueue_flag_ev_delete"

  let delete = delete ()

  external oneshot : unit -> int = "kqueue_flag_ev_oneshot"

  let oneshot = oneshot ()

  external clear : unit -> int = "kqueue_flag_ev_clear"

  let clear = clear ()

  external eof : unit -> int = "kqueue_flag_ev_eof"

  let eof = eof ()

  external error : unit -> int = "kqueue_flag_ev_error"

  let error = error ()

  let known =
    [ add, "EV_ADD"
    ; enable, "EV_ENABLE"
    ; disable, "EV_DISABLE"
    ; delete, "EV_DELETE"
    ; oneshot, "EV_ONESHOT"
    ; clear, "EV_CLEAR"
    ; eof, "EV_EOF"
    ; error, "EV_ERROR"
    ]
  ;;

  let pp fmt t =
    let known_flags =
      List.filter_map
        (fun (k, label) -> if is_subset k ~of_:t then Some label else None)
        known
    in
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
      Format.pp_print_string
      fmt
      known_flags
  ;;
end

module Filter = struct
  type t = int

  let equal a b = Int.equal a b
  let ( = ) = equal

  [%%if defined EVFILT_USER_AVAILABLE]

  external user : unit -> int = "kqueue_filter_evfilt_user"

  let user = user ()

  [%%endif]

  external read : unit -> int = "kqueue_filter_evfilt_read"

  let read = read ()

  external write : unit -> int = "kqueue_filter_evfilt_write"

  let write = write ()

  external timer : unit -> int = "kqueue_filter_evfilt_timer"

  let timer = timer ()

  external vnode : unit -> int = "kqueue_filter_evfilt_vnode"

  let vnode = vnode ()

  external proc : unit -> int = "kqueue_filter_evfilt_proc"

  let proc = proc ()

  let known_filters =
    [ read, "EVFILT_READ"
    ; write, "EVFILT_WRITE"
    ; timer, "EVFILT_TIMER"
    ; vnode, "EVFILT_VNODE"
    ]
  ;;

  [%%if defined EVFILT_USER_AVAILABLE]

  let known_filters = known_filters @ [ user, "EVFILT_USER" ]

  [%%endif]

  let to_string t =
    let rec loop filters =
      match filters with
      | [] -> Printf.sprintf "Unknown (%d)" t
      | (filter, label) :: _ when filter = t -> label
      | _ :: xs -> loop xs
    in
    loop known_filters
  ;;

  let pp fmt t = Format.fprintf fmt "%a" Format.pp_print_string (to_string t)
end

module Kevent = struct
  external sizeof : unit -> int = "kqueue_ml_kevent_sizeof"
  external event_ident_offset : unit -> int = "kqueue_ml_kevent_offset_event_fd"

  let event_ident_offset = event_ident_offset ()

  external event_filter_offset : unit -> int = "kqueue_ml_kevent_offset_filter"

  let event_filter_offset = event_filter_offset ()

  external event_flags_offset : unit -> int = "kqueue_ml_kevent_offset_flags"

  let event_flags_offset = event_flags_offset ()

  external event_fflags_offset : unit -> int = "kqueue_ml_kevent_offset_fflags"

  let event_fflags_offset = event_fflags_offset ()

  external event_data_offset : unit -> int = "kqueue_ml_kevent_offset_data"

  let event_data_offset = event_data_offset ()

  external event_udata_offset : unit -> int = "kqueue_ml_kevent_offset_udata"

  let event_udata_offset = event_udata_offset ()
  let sizeof = sizeof ()

  let read_ident_at buf idx =
    Bigstring.unsafe_get_int64_le_trunc buf ~pos:((idx * sizeof) + event_ident_offset)
  ;;

  let write_ident_at buf idx ident =
    Bigstring.unsafe_set_int64_le buf ~pos:((idx * sizeof) + event_ident_offset) ident
  ;;

  let read_filter_at buf idx =
    Bigstring.unsafe_get_int16_le buf ~pos:((idx * sizeof) + event_filter_offset)
  ;;

  let write_filter_at buf idx filter =
    Bigstring.unsafe_set_int16_le buf ~pos:((idx * sizeof) + event_filter_offset) filter
  ;;

  let read_flags_at buf idx =
    Bigstring.unsafe_get_int16_le buf ~pos:((idx * sizeof) + event_flags_offset)
  ;;

  let write_flags_at buf idx flags =
    Bigstring.unsafe_set_int16_le buf ~pos:((idx * sizeof) + event_flags_offset) flags
  ;;

  let read_fflags_at buf idx =
    Bigstring.unsafe_get_int32_le buf ~pos:((idx * sizeof) + event_fflags_offset)
  ;;

  let write_fflags_at buf idx fflags =
    Bigstring.unsafe_set_int32_le buf ~pos:((idx * sizeof) + event_fflags_offset) fflags
  ;;

  let read_data_at buf idx =
    Bigstring.unsafe_get_int64_le_trunc buf ~pos:((idx * sizeof) + event_data_offset)
  ;;

  let write_data_at buf idx data =
    Bigstring.unsafe_set_int64_le buf ~pos:((idx * sizeof) + event_data_offset) data
  ;;

  let read_udata_at buf idx =
    Bigstring.unsafe_get_int64_le_trunc buf ~pos:((idx * sizeof) + event_udata_offset)
  ;;

  let write_udata_at buf idx data =
    Bigstring.unsafe_set_int64_le buf ~pos:((idx * sizeof) + event_udata_offset) data
  ;;
end

module Event_list = struct
  type t = Bigstring.t

  let null = Bigstring.create 0

  module Event = struct
    type t =
      { buf : Bigstring.t
      ; idx : int
      }

    let get_ident t = Kevent.read_ident_at t.buf t.idx
    let set_ident t ident = Kevent.write_ident_at t.buf t.idx ident
    let get_filter t = Kevent.read_filter_at t.buf t.idx
    let set_filter t filter = Kevent.write_filter_at t.buf t.idx filter
    let get_flags t = Kevent.read_flags_at t.buf t.idx
    let set_flags t flags = Kevent.write_flags_at t.buf t.idx flags
    let get_fflags t = Kevent.read_fflags_at t.buf t.idx
    let set_fflags t fflags = Kevent.write_fflags_at t.buf t.idx fflags
    let get_data t = Kevent.read_data_at t.buf t.idx
    let set_data t data = Kevent.write_data_at t.buf t.idx data
    let get_udata t = Kevent.read_udata_at t.buf t.idx
    let set_udata t udata = Kevent.write_udata_at t.buf t.idx udata
  end

  let create size =
    if size < 1 then invalid_arg "Kqueue.create: changelist_size cannot be less than 1";
    Bigstring.create (Kevent.sizeof * size)
  ;;

  let get t idx = { Event.buf = t; idx }
end

module Timeout = struct
  type t = int64

  let never = -1L
  let immediate = 0L

  let of_ns x =
    if x < 0L then invalid_arg "Timeout cannot be negative";
    x
  ;;
end

type t =
  { kqueue_fd : Unix.file_descr
  ; mutable closed : bool
  }

let ensure_open t = if t.closed then failwith "Attempting to use a closed kqueue"
let create () = { kqueue_fd = Ffi.kqueue (); closed = false }

let kevent t ~changelist ~eventlist timeout =
  ensure_open t;
  Ffi.kevent t.kqueue_fd changelist eventlist timeout
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    Unix.close t.kqueue_fd)
;;

let available = true

[%%else]

include Null

[%%endif]
