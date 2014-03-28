module BigEndian = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

#include "src/be_ocaml_400.ml"
#include "src/common_float.ml"

end

module BigEndian_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

#include "src/be_ocaml_400.ml"
#include "src/common_float.ml"

end

module LittleEndian = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

#include "src/le_ocaml_400.ml"
#include "src/common_float.ml"

end

module LittleEndian_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

#include "src/le_ocaml_400.ml"
#include "src/common_float.ml"

end

#if ocaml_version >= (4, 0)
module NativeEndian = struct

  let get_char = get_char
  let get_uint8 = get_uint8
  let get_int8 = get_int8
  let set_char = set_char
  let set_int8 = set_int8

#include "src/ne_ocaml_400.ml"
#include "src/common_float.ml"

end

module NativeEndian_unsafe = struct

  let get_char = unsafe_get_char
  let get_uint8 = unsafe_get_uint8
  let get_int8 = unsafe_get_int8
  let set_char = unsafe_set_char
  let set_int8 = unsafe_set_int8

#include "src/ne_ocaml_400.ml"
#include "src/common_float.ml"

end
#endif
