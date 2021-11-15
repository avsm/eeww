module Bigstring = struct
  type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let create size = Bigarray.(Array1.create char c_layout size)

  external swap32 : int32 -> int32 = "%bswap_int32"
  external swap16 : int -> int = "%bswap16"
  external unsafe_get_int32 : t -> int -> int32 = "%caml_bigstring_get32u"
  external unsafe_get_int16 : t -> int -> int = "%caml_bigstring_get16u"

  let unsafe_get_int32_le t ~pos =
    Int32.to_int
      (if Sys.big_endian then swap32 (unsafe_get_int32 t pos) else unsafe_get_int32 t pos)
  ;;

  let sign_extend_16 u = (u lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

  let unsafe_get_int16_le t ~pos =
    if Sys.big_endian
    then sign_extend_16 (swap16 (unsafe_get_int16 t pos))
    else sign_extend_16 (unsafe_get_int16 t pos)
  ;;
end

module Fd = struct
  type t = Unix.file_descr

  let of_int : int -> Unix.file_descr = Obj.magic
end

module Filter = struct
  external evfilt_read : unit -> int = "kqueue_filter_evfilt_read"

  let evfilt_read = evfilt_read ()

  external evfilt_write : unit -> int = "kqueue_filter_evfilt_write"

  let evfilt_write = evfilt_write ()
end

module Flag = struct
  type t = int

  let ( + ) = ( lor )
  let is_subset t ~of_:flags = t = t land flags

  external ev_add : unit -> int = "kqueue_flag_ev_add"

  let ev_add = ev_add ()

  external ev_enable : unit -> int = "kqueue_flag_ev_enable"

  let ev_enable = ev_enable ()

  external ev_disable : unit -> int = "kqueue_flag_ev_disable"

  let ev_disable = ev_disable ()

  external ev_delete : unit -> int = "kqueue_flag_ev_delete"

  let ev_delete = ev_delete ()

  external ev_oneshot : unit -> int = "kqueue_flag_ev_oneshot"

  let ev_oneshot = ev_oneshot ()

  external ev_clear : unit -> int = "kqueue_flag_ev_clear"

  let ev_clear = ev_clear ()

  external ev_eof : unit -> int = "kqueue_flag_ev_eof"

  let ev_eof = ev_eof ()

  external ev_error : unit -> int = "kqueue_flag_ev_error"

  let ev_error = ev_error ()

  let known =
    [ ev_add, "EV_ADD"
    ; ev_enable, "EV_ENABLE"
    ; ev_disable, "EV_DISABLE"
    ; ev_delete, "EV_DELETE"
    ; ev_oneshot, "EV_ONESHOT"
    ; ev_clear, "EV_CLEAR"
    ; ev_eof, "EV_EOF"
    ; ev_error, "EV_ERROR"
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

module Kevent = struct
  external kevent_sizeof : unit -> int = "kqueue_ml_kevent_sizeof"

  let kevent_sizeof = kevent_sizeof ()

  external event_fd_offset : unit -> int = "kqueue_ml_kevent_offset_event_fd"

  let event_fd_offset = event_fd_offset ()

  external event_filter_offset : unit -> int = "kqueue_ml_kevent_offset_filter"

  let event_filter_offset = event_filter_offset ()

  external event_flags_offset : unit -> int = "kqueue_ml_kevent_offset_flags"

  let event_flags_offset = event_flags_offset ()

  let read_fd_at buf idx =
    Fd.of_int
      (Bigstring.unsafe_get_int32_le buf ~pos:((idx * kevent_sizeof) + event_fd_offset))
  ;;

  let read_filter_at buf idx =
    let filter =
      Bigstring.unsafe_get_int16_le buf ~pos:((idx * kevent_sizeof) + event_filter_offset)
    in
    if filter = Filter.evfilt_read
    then `Read
    else if filter = Filter.evfilt_write
    then `Write
    else (
      let msg =
        Printf.sprintf "Only read and write filters are handled for now %d" filter
      in
      (* TODO: handle more filters *)
      failwith msg)
  ;;

  let read_flags_at buf idx =
    Bigstring.unsafe_get_int16_le buf ~pos:((idx * kevent_sizeof) + event_flags_offset)
  ;;
end

type t =
  { kqueue_fd : Fd.t
  ; changelist_size : int
  ; mutable ready_events : int
  ; events : Bigstring.t
  }

external kqueue_create : unit -> Unix.file_descr = "kqueue_ml_kqueue_create"

let kqueue ~changelist_size =
  { kqueue_fd = kqueue_create ()
  ; changelist_size
  ; ready_events = 0
  ; events = Bigstring.create (Kevent.kevent_sizeof * changelist_size)
  }
;;

module Timeout = struct
  type t = int

  let never = -1
  let immediate = 0
  let of_ms ms = if ms < 0 then 0 else Int.min 1 ms
end

type event =
  [ `Read
  | `Write
  ]

let pp_event fmt t =
  let label =
    match t with
    | `Read -> "read"
    | `Write -> "write"
  in
  Format.pp_print_string fmt label
;;

let event_to_filter = function
  | `Read -> Filter.evfilt_read
  | `Write -> Filter.evfilt_write
;;

external kqueue_modify_fd : Fd.t -> Fd.t -> int -> int -> int = "kqueue_ml_modify_fd"
external kqueue_wait : Fd.t -> Bigstring.t -> int -> int = "kqueue_ml_wait"

let add t fd event =
  let filter = event_to_filter event in
  let flags = Flag.(ev_add + ev_oneshot) in
  ignore (kqueue_modify_fd t.kqueue_fd fd filter flags : int)
;;

let wait t timeout =
  t.ready_events <- 0;
  t.ready_events <- kqueue_wait t.kqueue_fd t.events timeout;
  if t.ready_events = 0 then `Timeout else `Ok
;;

let iter_ready t ~f =
  for i = 0 to t.ready_events - 1 do
    f
      (Kevent.read_fd_at t.events i)
      (Kevent.read_flags_at t.events i)
      (Kevent.read_filter_at t.events i)
  done
;;

let close t = Unix.close t.kqueue_fd
