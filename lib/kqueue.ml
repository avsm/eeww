open Core_kernel
module Unix = Caml_unix

module Fd = struct
  type t = Unix.file_descr

  let of_int : int -> Unix.file_descr = Obj.magic
  let to_int : Unix.file_descr -> int = Obj.magic
  let sexp_of_t t = sexp_of_int (to_int t)
end

module Filter = struct
  external evfilt_read : unit -> int = "kqueue_filter_evfilt_read"

  let evfilt_read = evfilt_read ()

  external evfilt_write : unit -> int = "kqueue_filter_evfilt_write"

  let evfilt_write = evfilt_write ()
end

module Flag = struct
  module F = struct
    external ev_add : unit -> int = "kqueue_flag_ev_add"

    let ev_add = Int63.of_int (ev_add ())

    external ev_enable : unit -> int = "kqueue_flag_ev_enable"

    let ev_enable = Int63.of_int (ev_enable ())

    external ev_disable : unit -> int = "kqueue_flag_ev_disable"

    let ev_disable = Int63.of_int (ev_disable ())

    external ev_delete : unit -> int = "kqueue_flag_ev_delete"

    let ev_delete = Int63.of_int (ev_delete ())

    external ev_oneshot : unit -> int = "kqueue_flag_ev_oneshot"

    let ev_oneshot = Int63.of_int (ev_oneshot ())

    external ev_clear : unit -> int = "kqueue_flag_ev_clear"

    let ev_clear = Int63.of_int (ev_clear ())

    external ev_eof : unit -> int = "kqueue_flag_ev_eof"

    let ev_eof = Int63.of_int (ev_eof ())

    external ev_error : unit -> int = "kqueue_flag_ev_error"

    let ev_error = Int63.of_int (ev_error ())

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

    let allow_intersecting = false
    let should_print_error = true
    let remove_zero_flags = false
  end

  include F
  include Flags.Make (F)
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
    Flag.of_int
      (Bigstring.unsafe_get_int16_le
         buf
         ~pos:((idx * kevent_sizeof) + event_flags_offset))
  ;;
end

type t =
  { kqueue_fd : Fd.t
  ; changelist_size : int
  ; mutable ready_events : int
  ; events : (Bigstring.t[@sexp.opaque])
  }
[@@deriving sexp_of]

external kqueue_create : unit -> Unix.file_descr = "kqueue_ml_kqueue_create"

let kqueue ~changelist_size =
  { kqueue_fd = kqueue_create ()
  ; changelist_size
  ; ready_events = 0
  ; events = Bigstring.create (Kevent.kevent_sizeof * changelist_size)
  }
;;

type event =
  [ `Read
  | `Write
  ]

let event_to_filter = function
  | `Read -> Filter.evfilt_read
  | `Write -> Filter.evfilt_write
;;

external kqueue_modify_fd : Fd.t -> Fd.t -> int -> int -> int = "kqueue_ml_modify_fd"
external kqueue_wait : Fd.t -> Bigstring.t -> int -> int = "kqueue_ml_wait"

let add t fd event =
  let filter = event_to_filter event in
  let flags = Flag.(to_int_exn @@ (ev_add + ev_oneshot)) in
  ignore (kqueue_modify_fd t.kqueue_fd fd filter flags : int)
;;

let wait t timeout =
  let timeout =
    if Time_ns.Span.( <= ) timeout Time_ns.Span.zero
    then 0
    else Time_ns.Span.to_int_ms (Time_ns.Span.min Time_ns.Span.millisecond timeout)
  in
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
