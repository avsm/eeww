(******************************************************************************
 * capnp-ocaml
 *
 * Copyright (c) 2013-2014, Paul Pelzl
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

(* Runtime support for Reader interfaces.  None of the functions provided
   here will modify the underlying message; derefencing null pointers and
   reading from truncated structs both lead to default data being returned. *)


open Core.Std

let sizeof_uint64 = 8

module RC = struct
  INCLUDE "common-inc.ml"
end
include RC

(* Given a pointer which is expected to be a list pointer, compute the
   corresponding list storage descriptor.  Returns None if the pointer is
   null. *)
let deref_list_pointer (pointer_bytes : 'cap Slice.t)
  : 'cap ListStorage.t option =
  match deref_pointer pointer_bytes with
  | Object.None ->
      None
  | Object.List list_descr ->
      Some list_descr
  | Object.Struct _ ->
      invalid_msg "decoded struct pointer where list pointer was expected"
  | Object.Capability _ ->
      invalid_msg "decoded capability pointer where list pointer was expected"


(* Given a pointer which is expected to be a struct pointer, compute the
   corresponding struct storage descriptor.  Returns None if the pointer is
   null. *)
let deref_struct_pointer (pointer_bytes : 'cap Slice.t)
  : 'cap StructStorage.t option =
  match deref_pointer pointer_bytes with
  | Object.None ->
      None
  | Object.Struct struct_descr ->
      Some struct_descr
  | Object.List _ ->
      invalid_msg "decoded list pointer where struct pointer was expected"
  | Object.Capability _ ->
      invalid_msg "decoded capability pointer where struct pointer was expected"


let void_list_decoders =
  ListDecoders.Empty (fun (x : unit) -> x)

let bit_list_decoders =
  ListDecoders.Bit (fun (x : bool) -> x)

let int8_list_decoders =
  ListDecoders.Bytes1 (fun slice -> Slice.get_int8 slice 0)

let int16_list_decoders =
  ListDecoders.Bytes2 (fun slice -> Slice.get_int16 slice 0)

let int32_list_decoders =
  ListDecoders.Bytes4 (fun slice -> Slice.get_int32 slice 0)

let int64_list_decoders =
  ListDecoders.Bytes8 (fun slice -> Slice.get_int64 slice 0)

let uint8_list_decoders =
  ListDecoders.Bytes1 (fun slice -> Slice.get_uint8 slice 0)

let uint16_list_decoders =
  ListDecoders.Bytes2 (fun slice -> Slice.get_uint16 slice 0)

let uint32_list_decoders =
  ListDecoders.Bytes4 (fun slice -> Slice.get_uint32 slice 0)

let uint64_list_decoders =
  ListDecoders.Bytes8 (fun slice -> Slice.get_uint64 slice 0)

let float32_list_decoders = ListDecoders.Bytes4
    (fun slice -> Int32.float_of_bits (Slice.get_int32 slice 0))

let float64_list_decoders = ListDecoders.Bytes8
    (fun slice -> Int64.float_of_bits (Slice.get_int64 slice 0))

let text_list_decoders = ListDecoders.Pointer (fun slice ->
    match deref_list_pointer slice with
    | Some list_storage ->
        string_of_uint8_list ~null_terminated:true list_storage
    | None ->
        "")

let blob_list_decoders = ListDecoders.Pointer (fun slice ->
    match deref_list_pointer slice with
    | Some list_storage ->
        string_of_uint8_list ~null_terminated:false list_storage
    | None ->
        "")

let struct_list_decoders =
  let struct_decoders =
    let bytes slice = Some {
        StructStorage.data = slice;
        StructStorage.pointers = {
          slice with
          Slice.start = Slice.get_end slice;
          Slice.len   = 0;
        };
      }
    in
    let pointer slice = Some {
        StructStorage.data = {
          slice with
          Slice.len = 0;
        };
        StructStorage.pointers = slice;
      }
    in
    let composite x = Some x in {
      ListDecoders.bytes;
      ListDecoders.pointer;
      ListDecoders.composite;
    }
  in
  ListDecoders.Struct struct_decoders


(* Locate the storage region corresponding to the root struct of a message. *)
let get_root_struct (m : 'cap Message.t) : 'cap StructStorage.t option =
  let first_segment = Message.get_segment m 0 in
  if Segment.length first_segment < sizeof_uint64 then
    None
  else
    let pointer_bytes = {
      Slice.msg        = m;
      Slice.segment    = first_segment;
      Slice.segment_id = 0;
      Slice.start      = 0;
      Slice.len        = sizeof_uint64
    } in
    deref_struct_pointer pointer_bytes


(*******************************************************************************
 * METHODS FOR GETTING OBJECTS STORED BY VALUE
 *******************************************************************************)

(* Given storage for a struct, attempt to get the bytes associated
   with the struct data section. *)
let get_data_region
    (struct_storage_opt : 'cap StructStorage.t option)
  : 'cap Slice.t option =
  match struct_storage_opt with
  | Some struct_storage ->
      Some struct_storage.StructStorage.data
  | None ->
      None


let get_void
    (data_opt : 'cap Slice.t option)
  : unit =
  ()

let get_bit
    ~(default : bool)
    ~(byte_ofs : int)
    ~(bit_ofs : int)
    (data_opt : 'cap Slice.t option)
  : bool =
  let byte_val =
    match data_opt with
    | Some data when byte_ofs < data.Slice.len ->
        Slice.get_uint8 data byte_ofs
    | _ ->
        0
  in
  let bit_val = (byte_val land (1 lsl bit_ofs)) <> 0 in
  if default then not bit_val else bit_val

let get_int8
    ~(default : int)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : int =
  let numeric =
    match data_opt with
    | Some data when byte_ofs < data.Slice.len ->
        Slice.get_int8 data byte_ofs
    | _ ->
        0
  in
  numeric lxor default

let get_int16
    ~(default : int)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : int =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 1 < data.Slice.len ->
        Slice.get_int16 data byte_ofs
    | _ ->
        0
  in
  numeric lxor default

let get_int32
    ~(default : int32)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
    : int32 =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 3 < data.Slice.len ->
        Slice.get_int32 data byte_ofs
    | _ ->
        Int32.zero
  in
  Int32.bit_xor numeric default

let get_int64
    ~(default : int64)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : int64 =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 7 < data.Slice.len ->
        Slice.get_int64 data byte_ofs
    | _ ->
        Int64.zero
  in
  Int64.bit_xor numeric default

let get_uint8
    ~(default : int)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : int =
  let numeric =
    match data_opt with
    | Some data when byte_ofs < data.Slice.len ->
        Slice.get_uint8 data byte_ofs
    | _ ->
        0
  in
  numeric lxor default

let get_uint16
    ~(default : int)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : int =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 1 < data.Slice.len ->
        Slice.get_uint16 data byte_ofs
    | _ ->
        0
  in
  numeric lxor default

let get_uint32
    ~(default : Uint32.t)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
    : Uint32.t =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 3 < data.Slice.len ->
        Slice.get_uint32 data byte_ofs
    | _ ->
        Uint32.zero
  in
  Uint32.logxor numeric default

let get_uint64
    ~(default : Uint64.t)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : Uint64.t =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 7 < data.Slice.len ->
        Slice.get_uint64 data byte_ofs
    | _ ->
        Uint64.zero
  in
  Uint64.logxor numeric default

let get_float32
    ~(default_bits : int32)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : float =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 3 < data.Slice.len ->
        Slice.get_int32 data byte_ofs
    | _ ->
        Int32.zero
  in
  let bits = Int32.bit_xor numeric default_bits in
  Int32.float_of_bits bits

let get_float64
    ~(default_bits : int64)
    ~(byte_ofs : int)
    (data_opt : 'cap Slice.t option)
  : float =
  let numeric =
    match data_opt with
    | Some data when byte_ofs + 7 < data.Slice.len ->
        Slice.get_int64 data byte_ofs
    | _ ->
        Int64.zero
  in
  let bits = Int64.bit_xor numeric default_bits in
  Int64.float_of_bits bits


(*******************************************************************************
 * METHODS FOR GETTING OBJECTS STORED BY POINTER
 *******************************************************************************)

let has_field
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : bool =
  match struct_storage_opt with
  | Some struct_storage ->
      let pointers = struct_storage.StructStorage.pointers in
      let start = pointer_word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        let pointer64 = Slice.get_int64 pointers start in
        not (Util.is_int64_zero pointer64)
      else
        false
  | None ->
      false

let get_text
    ~(default : string)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : string =
  match struct_storage_opt with
  | Some struct_storage ->
      let pointers = struct_storage.StructStorage.pointers in
      let start = pointer_word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        let pointer_bytes = {
          pointers with
          Slice.start = pointers.Slice.start + start;
          Slice.len   = len;
        } in
        match deref_list_pointer pointer_bytes with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:true list_storage
        | None ->
            default
      else
        default
  | None ->
      default

let get_blob
    ~(default : string)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : string =
  match struct_storage_opt with
  | Some struct_storage ->
      let pointers = struct_storage.StructStorage.pointers in
      let start = pointer_word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        let pointer_bytes = {
          pointers with
          Slice.start = pointers.Slice.start + start;
          Slice.len   = len;
        } in
        match deref_list_pointer pointer_bytes with
        | Some list_storage ->
            string_of_uint8_list ~null_terminated:false list_storage
        | None ->
            default
      else
        default
  | None ->
      default

let get_list
    ?(default : ro ListStorage.t option)
    (decoders : ('cap, 'a) ListDecoders.t)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, 'a, 'cap ListStorage.t) InnerArray.t =
  let make_default () =
    begin match default with
    | Some default_storage ->
        make_array_readonly default_storage decoders
    | None ->
        (* Empty array *)
        { InnerArray.length     = 0;
          InnerArray.storage    = None;
          InnerArray.init       = InnerArray.invalid_init;
          InnerArray.get_unsafe = InnerArray.invalid_get_unsafe;
          InnerArray.set_unsafe = InnerArray.invalid_set_unsafe; }
    end
  in
  match struct_storage_opt with
  | Some struct_storage ->
      let pointers = struct_storage.StructStorage.pointers in
      let start = pointer_word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        let pointer_bytes = {
          pointers with
          Slice.start = pointers.Slice.start + start;
          Slice.len   = len;
        } in
        match deref_list_pointer pointer_bytes with
        | Some list_storage ->
            make_array_readonly list_storage decoders
        | None ->
            make_default ()
      else
        make_default ()
  | None ->
      make_default ()

let get_void_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, unit, 'cap ListStorage.t) InnerArray.t =
  get_list ?default void_list_decoders struct_storage_opt pointer_word

let get_bit_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, bool, 'cap ListStorage.t) InnerArray.t =
  get_list ?default bit_list_decoders struct_storage_opt pointer_word

let get_int8_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, int, 'cap ListStorage.t) InnerArray.t =
  get_list ?default int8_list_decoders struct_storage_opt pointer_word

let get_int16_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, int, 'cap ListStorage.t) InnerArray.t =
  get_list ?default int16_list_decoders struct_storage_opt pointer_word

let get_int32_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, int32, 'cap ListStorage.t) InnerArray.t =
  get_list ?default int32_list_decoders struct_storage_opt pointer_word

let get_int64_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, int64, 'cap ListStorage.t) InnerArray.t =
  get_list ?default int64_list_decoders struct_storage_opt pointer_word

let get_uint8_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, int, 'cap ListStorage.t) InnerArray.t =
  get_list ?default uint8_list_decoders struct_storage_opt pointer_word

let get_uint16_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, int, 'cap ListStorage.t) InnerArray.t =
  get_list ?default uint16_list_decoders struct_storage_opt pointer_word

let get_uint32_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, Uint32.t, 'cap ListStorage.t) InnerArray.t =
  get_list ?default uint32_list_decoders struct_storage_opt pointer_word

let get_uint64_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, Uint64.t, 'cap ListStorage.t) InnerArray.t =
  get_list ?default uint64_list_decoders struct_storage_opt pointer_word

let get_float32_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, float, 'cap ListStorage.t) InnerArray.t =
  get_list ?default float32_list_decoders struct_storage_opt pointer_word

let get_float64_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, float, 'cap ListStorage.t) InnerArray.t =
  get_list ?default float64_list_decoders struct_storage_opt pointer_word

let get_text_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, string, 'cap ListStorage.t) InnerArray.t =
  get_list ?default text_list_decoders struct_storage_opt pointer_word

let get_blob_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, string, 'cap ListStorage.t) InnerArray.t =
  get_list ?default blob_list_decoders struct_storage_opt pointer_word

let get_struct_list
    ?(default : ro ListStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : (ro, 'cap StructStorage.t option, 'cap ListStorage.t) InnerArray.t =
  get_list ?default struct_list_decoders struct_storage_opt pointer_word

let get_struct
    ?(default : ro StructStorage.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : 'cap StructStorage.t option =
  match struct_storage_opt with
  | Some struct_storage ->
      let pointers = struct_storage.StructStorage.pointers in
      let start = pointer_word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        let pointer_bytes = {
          pointers with
          Slice.start = pointers.Slice.start + start;
          Slice.len   = len;
        } in
        match deref_struct_pointer pointer_bytes with
        | Some storage ->
            Some storage
        | None ->
            default
      else
        default
  | None ->
      default

let get_pointer
    ?(default: ro Slice.t option)
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : 'cap Slice.t option =
  match struct_storage_opt with
  | Some struct_storage ->
      let pointers = struct_storage.StructStorage.pointers in
      let start = pointer_word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        let pointer64 = Slice.get_int64 pointers start in
        if Util.is_int64_zero pointer64 then
          default
        else
          let pointer_bytes = {
            pointers with
            Slice.start = pointers.Slice.start + start;
            Slice.len   = len;
          } in
          Some pointer_bytes
      else
        default
  | None ->
      default

let get_interface
    (struct_storage_opt : 'cap StructStorage.t option)
    (pointer_word : int)
  : Uint32.t option =
  match struct_storage_opt with
  | Some struct_storage ->
      let pointers = struct_storage.StructStorage.pointers in
      let start = pointer_word * sizeof_uint64 in
      let len   = sizeof_uint64 in
      if start + len <= pointers.Slice.len then
        let pointer_bytes = {
          pointers with
          Slice.start = pointers.Slice.start + start;
          Slice.len   = len;
        } in
        match decode_pointer pointer_bytes with
        | Pointer.Null ->
            None
        | Pointer.Other (OtherPointer.Capability index) ->
            Some index
        | _ ->
            invalid_msg "decoded non-capability pointer where capability was expected"
      else
        None
  | None ->
      None

