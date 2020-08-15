open Import
module C = Constants

type t =
  | Add
  | Enable
  | Disable
  | Dispatch
  | Delete
  | Receipt
  | Oneshot
  | Clear
  | EOF
  | Error

let all =
  [Add; Enable; Disable; Dispatch; Delete; Receipt; Oneshot; Clear; EOF; Error]

let to_uint = function
  | Add ->
      C.ev_add
  | Enable ->
      C.ev_enable
  | Disable ->
      C.ev_disable
  | Dispatch ->
      C.ev_dispatch
  | Delete ->
      C.ev_delete
  | Receipt ->
      C.ev_receipt
  | Oneshot ->
      C.ev_oneshot
  | Clear ->
      C.ev_clear
  | EOF ->
      C.ev_eof
  | Error ->
      C.ev_error

let flags_to_uint flags =
  List.fold_left
    (fun acc flag -> Unsigned.UInt16.logor acc (to_uint flag))
    Unsigned.UInt16.zero flags

let flags_of_uint flags =
  List.filter
    (fun flag ->
      let uint = to_uint flag in
      Unsigned.UInt16.logand uint flags = uint)
    all
