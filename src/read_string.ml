(************************************************************************)
(*  ocp-read                                                            *)
(*                                                                      *)
(*    Copyright 2012 OCamlPro                                           *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU Lesser General  *)
(*  Public License as published by the Free Software Foundation; either *)
(*  version 2.1 of the License, or (at your option) any later version,  *)
(*  with the OCaml static compilation exception.                        *)
(*                                                                      *)
(*  ocp-read is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

let get_char (s:string) off =
  String.get s off
let set_char (s:string) off v =
  String.set s off v
let unsafe_get_char (s:string) off =
  String.unsafe_get s off
let unsafe_set_char (s:string) off v =
  String.unsafe_set s off v

#include "src/common.ml"

#if ocaml_version >= (4, 1)

external unsafe_get_16 : string -> int -> int = "%caml_string_get16u"
external unsafe_get_32 : string -> int -> int32 = "%caml_string_get32u"
external unsafe_get_64 : string -> int -> int64 = "%caml_string_get64u"

external unsafe_set_16 : string -> int -> int -> unit = "%caml_string_set16u"
external unsafe_set_32 : string -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_set_64 : string -> int -> int64 -> unit = "%caml_string_set64u"

external get_16 : string -> int -> int = "%caml_string_get16"
external get_32 : string -> int -> int32 = "%caml_string_get32"
external get_64 : string -> int -> int64 = "%caml_string_get64"

external set_16 : string -> int -> int -> unit = "%caml_string_set16"
external set_32 : string -> int -> int32 -> unit = "%caml_string_set32"
external set_64 : string -> int -> int64 -> unit = "%caml_string_set64"

#include "src/common_401.ml"

#else

#include "src/common_400.ml"

#endif
