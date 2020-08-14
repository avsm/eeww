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
[@@deriving sexp]

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
