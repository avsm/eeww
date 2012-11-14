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

let sign8 v =
  (v lsl ( Sys.word_size - 9 )) asr ( Sys.word_size - 9 )

let sign16 v =
  (v lsl ( Sys.word_size - 17 )) asr ( Sys.word_size - 17 )

let get_char (s:bigstring) off =
  Array1.get s off
let get_uint8 (s:bigstring) off =
  Char.code (Array1.get s off)
let get_int8 s off =
  sign8 (get_uint8 s off)
let set_char (s:bigstring) off v =
  Array1.set s off v
let set_int8 (s:bigstring) off v =
  set_char s off (Char.chr v)

let unsafe_get_char (s:bigstring) off =
  Array1.unsafe_get s off
let unsafe_get_uint8 (s:bigstring) off =
  Char.code (Array1.unsafe_get s off)
let unsafe_get_int8 s off =
  sign8 (unsafe_get_uint8 s off)
let unsafe_set_char (s:bigstring) off v =
  Array1.unsafe_set s off v
let unsafe_set_int8 (s:bigstring) off v =
  unsafe_set_char s off (Char.chr v)

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

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external swapnative : nativeint -> nativeint = "%bswap_native"

module BE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

#include "src/be_ocaml_401.ml"

end

module BE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

#include "src/be_ocaml_401.ml"

end

module LE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

#include "src/le_ocaml_401.ml"

end

module LE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

#include "src/le_ocaml_401.ml"

end

#else

module BE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

#include "src/be_ocaml_400.ml"

end

module BE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

#include "src/be_ocaml_400.ml"

end

module LE = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

#include "src/le_ocaml_400.ml"

end

module LE_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

#include "src/le_ocaml_400.ml"

end

#endif
