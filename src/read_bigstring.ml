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

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

let get_char (s:bigstring) off =
  Array1.get s off
let set_char (s:bigstring) off v =
  Array1.set s off v
let unsafe_get_char (s:bigstring) off =
  Array1.unsafe_get s off
let unsafe_set_char (s:bigstring) off v =
  Array1.unsafe_set s off v

#include "src/common.ml"

#if ocaml_version >= (4, 1)

external unsafe_get_16 : bigstring -> int -> int = "%caml_bigstring_get16u"
external unsafe_get_32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_get_64 : bigstring -> int -> int64 = "%caml_bigstring_get64u"

external unsafe_set_16 : bigstring -> int -> int -> unit = "%caml_bigstring_set16u"
external unsafe_set_32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"
external unsafe_set_64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64u"

external get_16 : bigstring -> int -> int = "%caml_bigstring_get16"
external get_32 : bigstring -> int -> int32 = "%caml_bigstring_get32"
external get_64 : bigstring -> int -> int64 = "%caml_bigstring_get64"

external set_16 : bigstring -> int -> int -> unit = "%caml_bigstring_set16"
external set_32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32"
external set_64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64"

#include "src/common_401.ml"

#else

#include "src/common_400.ml"

#endif
