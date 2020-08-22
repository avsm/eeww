module Bindings = Kqueue_stubs.Definition (Kqueue_generated_stubs)

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

let to_unix_error = function
  | `ENOMEM ->
      Unix.ENOMEM
  | `EMFILE ->
      Unix.EMFILE
  | `ENFILE ->
      Unix.ENFILE
  | `EACCES ->
      Unix.EACCES
  | `EFAULT ->
      Unix.EFAULT
  | `EBADF ->
      Unix.EBADF
  | `EINTR ->
      Unix.EINTR
  | `EINVAL ->
      Unix.EINVAL
  | `ENOENT ->
      Unix.ENOENT
  | `ESRCH ->
      Unix.ESRCH
  | `UNKNOWN i ->
      Unix.EUNKNOWNERR i

let error_message e = Unix.error_message (to_unix_error e)

let of_c_error e =
  let open Bindings in
  match e with
  | e when e = enomem ->
      `ENOMEM
  | e when e = emfile ->
      `EMFILE
  | e when e = enfile ->
      `ENFILE
  | e when e = eacces ->
      `EACCES
  | e when e = efault ->
      `EFAULT
  | e when e = ebadf ->
      `EBADF
  | e when e = eintr ->
      `EINTR
  | e when e = einval ->
      `EINVAL
  | e when e = enoent ->
      `ENOENT
  | e when e = esrch ->
      `ESRCH
  | e ->
      `UNKNOWN e

let setf = Ctypes.setf

let getf = Ctypes.getf

module Timespec = struct
  type t = Bindings.Timespec.t

  let sec t = PosixTypes.Time.to_int64 (Ctypes.getf t Bindings.Timespec.tv_sec)

  let nsec t = Signed.Long.to_int64 (Ctypes.getf t Bindings.Timespec.tv_nsec)

  let nsec_per_sec = 1_000_000_000L

  let rec normalize_timespec ~sec ~nsec =
    if nsec > nsec_per_sec then
      normalize_timespec ~sec:(Int64.succ sec)
        ~nsec:(Int64.sub nsec nsec_per_sec)
    else if nsec < Int64.zero then
      normalize_timespec ~sec:(Int64.pred sec)
        ~nsec:(Int64.add nsec nsec_per_sec)
    else (sec, nsec)

  let make ~sec ~nsec =
    let sec, nsec = normalize_timespec ~sec ~nsec in
    let t = Ctypes.make Bindings.Timespec.t in
    setf t Bindings.Timespec.tv_sec (PosixTypes.Time.of_int64 sec) ;
    setf t Bindings.Timespec.tv_nsec (Signed.Long.of_int64 nsec) ;
    t
end

module Kevent = struct
  type t = Bindings.Kevent.t

  let make ~ident ~filter ~flags ~fflags ~data ~udata =
    let t = Ctypes.make Bindings.Kevent.t in
    setf t Bindings.Kevent.ident ident ;
    setf t Bindings.Kevent.filter filter ;
    setf t Bindings.Kevent.flags flags ;
    setf t Bindings.Kevent.fflags fflags ;
    setf t Bindings.Kevent.data data ;
    setf t Bindings.Kevent.udata udata ;
    t

  let ident t = getf t Bindings.Kevent.ident

  let filter t = getf t Bindings.Kevent.filter

  let flags t = getf t Bindings.Kevent.flags

  let fflags t = getf t Bindings.Kevent.fflags

  let data t = getf t Bindings.Kevent.data

  let udata t = getf t Bindings.Kevent.udata
end

type t = int

let kqueue () =
  let q, err = Bindings.kqueue () in
  if q = -1 then Error (of_c_error (Signed.SInt.to_int err)) else Ok q

let kqueue_exn () =
  match kqueue () with
  | Ok q ->
      q
  | Error e ->
      raise (Unix.Unix_error (to_unix_error e, "kqueue", ""))

let kevent ?timeout q ~changelist ~eventlist =
  let open Ctypes in
  let timespec =
    match timeout with
    | Some t ->
        addr t
    | None ->
        coerce (ptr void) (ptr Bindings.Timespec.t) null
  in
  let res, err =
    Bindings.kevent q (CArray.start changelist) (CArray.length changelist)
      (CArray.start eventlist) (CArray.length eventlist) timespec
  in
  if res = -1 then Error (of_c_error (Signed.SInt.to_int err)) else Ok res

let kevent_exn ?timeout q ~changelist ~eventlist =
  match kevent ?timeout q ~changelist ~eventlist with
  | Ok res ->
      res
  | Error e ->
      raise (Unix.Unix_error (to_unix_error e, "kevent", ""))
