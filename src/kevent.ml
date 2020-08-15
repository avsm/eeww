open Import

type t =
  { ident: int
  ; filter: int
  ; flags: Flag.t list
  ; fflags: int
  ; data: int
  ; udata: int }

let to_cstruct {ident; filter; flags; fflags; data; udata} =
  let open Ctypes in
  let kevent = make Bindings.Kevent.t in
  setf kevent Bindings.Kevent.ident ident ;
  setf kevent Bindings.Kevent.filter filter ;
  setf kevent Bindings.Kevent.flags (Flag.flags_to_uint flags) ;
  setf kevent Bindings.Kevent.fflags (Unsigned.UInt32.of_int fflags) ;
  setf kevent Bindings.Kevent.data (Intptr.of_int data) ;
  setf kevent Bindings.Kevent.udata (Uintptr.of_int udata) ;
  kevent
