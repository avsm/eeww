open Import

type t =
  { ident: int
  ; filter: int
  ; flags: Flag.t list
  ; fflags: int
  ; data: int
  ; udata: int }

let ident t = t.ident

let filter t = t.filter

let flags t = t.flags

let fflags t = t.fflags

let data t = t.data

let udata t = t.udata

let make ~ident ~filter ~flags ~fflags ~data ~udata =
  {ident; filter; flags; fflags; data; udata}

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

let of_cstruct cstr =
  let getf = Ctypes.getf in
  let ident = getf cstr Bindings.Kevent.ident in
  let filter = getf cstr Bindings.Kevent.filter in
  let flags = getf cstr Bindings.Kevent.flags |> Flag.flags_of_uint in
  let fflags = getf cstr Bindings.Kevent.fflags |> Unsigned.UInt32.to_int in
  let data = getf cstr Bindings.Kevent.data |> Ctypes.Intptr.to_int in
  let udata = getf cstr Bindings.Kevent.udata |> Ctypes.Uintptr.to_int in
  make ~ident ~filter ~flags ~fflags ~data ~udata
