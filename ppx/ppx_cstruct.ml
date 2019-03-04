(*
 * Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Migrate_parsetree
open Printf

open Ast_404
open Longident
open Asttypes
open Parsetree
open Ast_helper
open Ast_mapper
module Loc = Location
module Ast = Ast_convenience_404

type mode = Big_endian | Little_endian | Host_endian | Bi_endian

type prim =
  | Char
  | UInt8
  | UInt16
  | UInt32
  | UInt64

type ty =
  | Prim of prim
  | Buffer of prim * int

type field = {
  field: string;
  ty: ty;
  off: int;
}

let field_is_ignored f =
  String.get f.field 0 = '_'

type t = {
  name: string;
  fields: field list;
  len: int;
  endian: mode;
}

let ty_of_string =
  function
  |"char_t"  |"char" -> Some Char
  |"uint8_t" |"uint8" |"int8" |"int8_t"  -> Some UInt8
  |"uint16_t"|"uint16"|"int16"|"int16_t" -> Some UInt16
  |"uint32_t"|"uint32"|"int32"|"int32_t" -> Some UInt32
  |"uint64_t"|"uint64"|"int64"|"int64_t" -> Some UInt64
  |_ -> None

let width_of_field f =
  let rec width = function
    |Prim Char -> 1
    |Prim UInt8 -> 1
    |Prim UInt16 -> 2
    |Prim UInt32 -> 4
    |Prim UInt64 -> 8
    |Buffer (prim, len) -> (width (Prim prim)) * len
  in
  width f.ty

let field_to_string f =
  let rec string = function
    |Prim Char -> "char_t"
    |Prim UInt8 -> "uint8_t"
    |Prim UInt16 -> "uint16_t"
    |Prim UInt32 -> "uint32_t"
    |Prim UInt64 -> "uint64_t"
    |Buffer (prim, len) -> sprintf "%s[%d]" (string (Prim prim)) len
  in
  sprintf "%s %s" (string f.ty) f.field

let loc_err loc fmt = Location.raise_errorf ~loc ("ppx_cstruct error: " ^^ fmt)

let parse_field loc field field_type sz =
  match ty_of_string field_type with
  |None -> loc_err loc "Unknown type %s" field_type
  |Some ty -> begin
    let ty = match ty,sz with
      |_,None -> Prim ty
      |prim,Some sz -> Buffer (prim, sz)
    in
    let off = -1 in
    { field; ty; off }
  end

let create_struct loc endian name fields =
  let endian = match endian with
    |"little_endian" -> Little_endian
    |"big_endian" -> Big_endian
    |"host_endian" -> Host_endian
    |"bi_endian" -> Bi_endian
    |_ -> loc_err loc "unknown endian %s, should be little_endian, big_endian, host_endian or bi_endian" endian
  in
  let len, fields =
    List.fold_left (fun (off,acc) field ->
      let field = {field with off=off} in
      let off = width_of_field field + off in
      let acc = acc @ [field] in
      (off, acc)
    ) (0,[]) fields
  in
  { fields; name = name.txt; len; endian }

let ($.) l x = Longident.Ldot (l, x)
let cstruct_id = Longident.Lident "Cstruct"
let mode_mod s = function
  |Big_endian -> cstruct_id$."BE"$.s
  |Little_endian -> cstruct_id$."LE"$.s
  |Host_endian -> cstruct_id$."HE"$.s
  |Bi_endian -> cstruct_id$."BL"$.s

let mode_mod loc x s =
  Exp.ident ~loc {loc ; txt = mode_mod s x}

type op =
  | Op_get of field
  | Op_set of field
  | Op_copy of field
  | Op_blit of field
  | Op_sizeof
  | Op_hexdump
  | Op_hexdump_to_buffer

let op_name s op =
  let parts =
    match op with
    | Op_get f -> ["get"; s.name; f.field]
    | Op_set f -> ["set"; s.name; f.field]
    | Op_copy f -> ["copy"; s.name; f.field]
    | Op_blit f -> ["blit"; s.name; f.field]
    | Op_sizeof -> ["sizeof"; s.name]
    | Op_hexdump -> ["hexdump"; s.name]
    | Op_hexdump_to_buffer -> ["hexdump"; s.name; "to_buffer"]
  in
  String.concat "_" parts

let op_pvar s op = Ast.pvar (op_name s op)
let op_evar s op = Ast.evar (op_name s op)

let get_expr loc s f =
  let m = mode_mod loc s.endian in
  let num x = Ast.int x in
  match f.ty with
  |Buffer (_, _) ->
    let len = width_of_field f in
    [%expr
      fun src -> Cstruct.sub src [%e num f.off] [%e num len]
    ]
  |Prim prim ->
    [%expr
      fun v ->
        [%e match prim with
            |Char -> [%expr Cstruct.get_char v [%e num f.off]]
            |UInt8 -> [%expr Cstruct.get_uint8 v [%e num f.off]]
            |UInt16 -> [%expr [%e m "get_uint16"] v [%e num f.off]]
            |UInt32 -> [%expr [%e m "get_uint32"] v [%e num f.off]]
            |UInt64 -> [%expr [%e m "get_uint64"] v [%e num f.off]]]]

let type_of_int_field = function
  |Char -> [%type: char]
  |UInt8 -> [%type: Cstruct.uint8]
  |UInt16 -> [%type: Cstruct.uint16]
  |UInt32 -> [%type: Cstruct.uint32]
  |UInt64 -> [%type: Cstruct.uint64]

let set_expr loc s f =
  let m = mode_mod loc s.endian in
  let num x = Ast.int x in
  match f.ty with
  |Buffer (_,_) ->
    let len = width_of_field f in
    [%expr
      fun src srcoff dst ->
        Cstruct.blit_from_string src srcoff dst [%e num f.off] [%e num len]]
  |Prim prim ->
    [%expr fun v x ->
        [%e match prim with
            |Char -> [%expr Cstruct.set_char v [%e num f.off] x]
            |UInt8 -> [%expr Cstruct.set_uint8 v [%e num f.off] x]
            |UInt16 -> [%expr [%e m "set_uint16"] v [%e num f.off] x]
            |UInt32 -> [%expr [%e m "set_uint32"] v [%e num f.off] x]
            |UInt64 -> [%expr [%e m "set_uint64"] v [%e num f.off] x]]]

let type_of_set f =
  match f.ty with
  |Buffer (_,_) ->
    [%type: string -> int -> Cstruct.t -> unit]
  |Prim prim ->
    let retf = type_of_int_field prim in
    [%type: Cstruct.t -> [%t retf] -> unit]

let hexdump_expr s =
  [%expr fun v ->
    let buf = Buffer.create 128 in
    Buffer.add_string buf [%e Ast.str (s.name ^ " = {\n")];
    [%e op_evar s Op_hexdump_to_buffer] buf v;
    print_endline (Buffer.contents buf);
    print_endline "}"
  ]

let hexdump_to_buffer_expr s =
  let prim_format_string = function
    | Char -> [%expr "%c\n"]
    | UInt8 | UInt16 -> [%expr "0x%x\n"]
    | UInt32 -> [%expr "0x%lx\n"]
    | UInt64 -> [%expr "0x%Lx\n"]
  in
  let hexdump_field f =
    if field_is_ignored f then
      [%expr ()]
    else
      let get_f = op_evar s (Op_get f) in
      let expr =
        match f.ty with
        |Prim p ->
          [%expr Printf.bprintf buf [%e prim_format_string p] ([%e get_f] v)]
        |Buffer (_,_) ->
          [%expr Printf.bprintf buf "<buffer %s>" [%e Ast.str (field_to_string f)];
            Cstruct.hexdump_to_buffer buf ([%e get_f] v)]
    in
    [%expr
      Printf.bprintf buf "  %s = " [%e Ast.str f.field];
      [%e expr]]
  in
  [%expr fun buf v -> [%e Ast.sequence (List.map hexdump_field s.fields)]]

let op_expr loc s = function
  | Op_sizeof -> Ast.int s.len
  | Op_hexdump -> hexdump_expr s
  | Op_hexdump_to_buffer -> hexdump_to_buffer_expr s
  | Op_get f -> get_expr loc s f
  | Op_set f -> set_expr loc s f
  | Op_copy f ->
    let len = width_of_field f in
    [%expr fun src -> Cstruct.copy src [%e Ast.int f.off] [%e Ast.int len] ]
  | Op_blit f ->
    let len = width_of_field f in
    [%expr fun src srcoff dst ->
      Cstruct.blit src srcoff dst [%e Ast.int f.off] [%e Ast.int len]]

let field_ops_for f =
  if field_is_ignored f then
    []
  else
    let if_buffer x =
      match f.ty with
      |Buffer (_,_) -> [x]
      |Prim _ -> []
    in
    List.concat
      [ [Op_get f]
      ; if_buffer (Op_copy f)
      ; [Op_set f]
      ; if_buffer (Op_blit f)
      ]

let ops_for s =
  ( [Op_sizeof]
  @ List.concat (List.map field_ops_for s.fields)
  @ [Op_hexdump_to_buffer;
     Op_hexdump;
    ])

(** Generate functions of the form {get/set}_<struct>_<field> *)
let output_struct_one_endian loc s =
  List.map
    (fun op ->
       [%stri let[@ocaml.warning "-32"] [%p op_pvar s op] =
                [%e op_expr loc s op]])
    (ops_for s)

let output_struct _loc s =
  match s.endian with
  | Bi_endian ->
    (* In case of Bi-endian, create two modules - one for BE and one for LE *)
    let expr_be = Mod.structure (output_struct_one_endian _loc {s with endian = Big_endian})
    and expr_le = Mod.structure (output_struct_one_endian _loc {s with endian = Little_endian})

    in [{pstr_desc = Pstr_module
        {pmb_name = {txt = "BE"; loc = _loc}; pmb_expr = expr_be;
        pmb_attributes = []; pmb_loc = _loc;}; pstr_loc = _loc;};
        {pstr_desc = Pstr_module
        {pmb_name = {txt = "LE"; loc = _loc}; pmb_expr = expr_le;
        pmb_attributes = []; pmb_loc = _loc;}; pstr_loc = _loc;}
        ]
  | _ -> output_struct_one_endian _loc s

let type_of_get f =
  match f.ty with
  |Buffer (_,_) ->
    [%type: Cstruct.t -> Cstruct.t]
  |Prim prim ->
    let retf = type_of_int_field prim in
    [%type: Cstruct.t -> [%t retf]]

let op_typ = function
  | Op_sizeof -> [%type: int]
  | Op_hexdump_to_buffer -> [%type: Buffer.t -> Cstruct.t -> unit]
  | Op_hexdump -> [%type: Cstruct.t -> unit]
  | Op_get f -> type_of_get f
  | Op_set f -> type_of_set f
  | Op_copy _ -> [%type: Cstruct.t -> string]
  | Op_blit _ -> [%type: Cstruct.t -> int -> Cstruct.t -> unit]

(** Generate signatures of the form {get/set}_<struct>_<field> *)
let output_struct_sig loc s =
  List.map
    (fun op ->
       Sig.value
         (Val.mk
            (Loc.mkloc (op_name s op) loc)
            (op_typ op)))
    (ops_for s)

let convert_integer f suffix =
  (fun i -> Exp.constant (Pconst_integer(f i, suffix))),
  (fun i -> Pat.constant (Pconst_integer(f i, suffix)))

let output_enum _loc name fields width ~sexp =
  let intfn,pattfn = match ty_of_string width with
    |None -> loc_err _loc "enum: unknown width specifier %s" width
    |Some Char ->
      (fun i -> Ast.char (Char.chr (Int64.to_int i))),
      (fun i -> Ast.pchar (Char.chr (Int64.to_int i)))
    |Some (UInt8 | UInt16) -> convert_integer Int64.to_string None
    |Some UInt32 -> convert_integer (fun i -> Int32.to_string (Int64.to_int32 i)) (Some 'l')
    |Some UInt64 -> convert_integer Int64.to_string (Some 'L')
  in
  let decls = List.map (fun (f,_) -> Type.constructor f) fields in
  let getters = (List.map (fun ({txt = f; _},i) ->
    Exp.case (pattfn i) [%expr Some [%e Ast.constr f []]]
    ) fields) @ [Exp.case [%pat? _] [%expr None]] in
  let setters = List.map (fun ({txt = f; _},i) ->
      Exp.case (Ast.pconstr f []) (intfn i)
    ) fields in
  let printers = List.map (fun ({txt = f; _},_) ->
      Exp.case (Ast.pconstr f []) (Ast.str f)
    ) fields in
  let parsers = List.map (fun ({txt = f; _},_) ->
      Exp.case (Ast.pstr f) [%expr Some [%e Ast.constr f []]]
    ) fields in
  let getter {txt = x; _} = sprintf "int_to_%s" x in
  let setter {txt = x; _} = sprintf "%s_to_int" x in
  let printer {txt = x; _} = sprintf "%s_to_string" x in
  let parse {txt = x; _} = sprintf "string_to_%s" x in
  let of_sexp {txt = x; _} = sprintf "%s_of_sexp" x in
  let to_sexp {txt = x; _} = sprintf "sexp_of_%s" x in
  let declare name expr =
    [%stri let[@ocaml.warning "-32"] [%p Ast.pvar name] = fun x -> [%e expr]]
  in
  let output_sexp_struct =
    [ declare (to_sexp name)
        [%expr Sexplib.Sexp.Atom ([%e Ast.evar (printer name)] x) ]
    ; declare (of_sexp name)
        [%expr
          match x with
          | Sexplib.Sexp.List _ ->
            raise (Sexplib.Pre_sexp.Of_sexp_error (Failure "expected Atom, got List", x))
          | Sexplib.Sexp.Atom v ->
            match [%e Ast.evar (parse name)] v with
            | None ->
              raise (Sexplib.Pre_sexp.Of_sexp_error (Failure "unable to parse enum string", x))
            | Some r -> r
        ]
      ] in
  Str.type_ Recursive [Type.mk ~kind:(Ptype_variant decls) name] ::
  declare (getter name) (Exp.match_ [%expr x] getters) ::
  declare (setter name) (Exp.match_ [%expr x] setters) ::
  declare (printer name) (Exp.match_ [%expr x] printers) ::
  declare (parse name)
    (Exp.match_ [%expr x]
       (parsers @ [Exp.case [%pat? _] [%expr None]])
    ) ::
  if sexp then output_sexp_struct else []

let output_enum_sig _loc name fields width ~sexp =
  let oty = match ty_of_string width with
    |None -> loc_err _loc "enum: unknown width specifier %s" width
    |Some Char -> [%type: char]
    |Some (UInt8|UInt16) -> [%type: int]
    |Some UInt32 -> [%type: int32]
    |Some UInt64 -> [%type: int64]
  in
  let decls = List.map (fun (f,_) -> Type.constructor f) fields in
  let getter {txt = x; _}  = sprintf "int_to_%s" x in
  let setter {txt = x; _}  = sprintf "%s_to_int" x in
  let printer {txt = x; _} = sprintf "%s_to_string" x in
  let parse {txt = x; _}   = sprintf "string_to_%s" x in
  let of_sexp {txt = x; _} = sprintf "%s_of_sexp" x in
  let to_sexp {txt = x; _} = sprintf "sexp_of_%s" x in
  let ctyo = [%type: [%t Ast.tconstr name.txt []] option] in
  let cty = Ast.tconstr name.txt [] in
  let output_sexp_sig =
    [
      Sig.value (Val.mk (Loc.mkloc (to_sexp name) _loc) [%type: [%t cty] -> Sexplib.Sexp.t]);
      Sig.value (Val.mk (Loc.mkloc (of_sexp name) _loc) [%type: Sexplib.Sexp.t -> [%t cty]])
    ]
  in
  Sig.type_ Recursive [Type.mk ~kind:(Ptype_variant decls) name] ::
  Sig.value (Val.mk (Loc.mkloc (getter name) _loc) [%type: [%t oty] -> [%t ctyo]]) ::
  Sig.value (Val.mk (Loc.mkloc (setter name) _loc) [%type: [%t cty] -> [%t oty]]) ::
  Sig.value (Val.mk (Loc.mkloc (printer name) _loc) [%type: [%t cty] -> string]) ::
  Sig.value (Val.mk (Loc.mkloc (parse name) _loc) [%type: string -> [%t cty] option]) ::
  if sexp then output_sexp_sig else []

let constr_enum = function
  | {pcd_name = f; pcd_args = Pcstr_tuple []; pcd_attributes = attrs; _} ->
    let id = match attrs with
      | [{txt = "id"; _}, PStr
           [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant cst; pexp_loc = loc; _}, _); _}]] ->
        let cst = match cst with
          | Pconst_integer(i, _) -> Int64.of_string i
          | _ ->
            loc_err loc "invalid id"
        in
        Some cst
      | _ ->
        None
    in
    (f, id)
  | {pcd_loc = loc; _} ->
    loc_err loc "invalid cenum variant"

let constr_field {pld_name = fname; pld_type = fty; pld_loc = loc; pld_attributes = att; _} =
  let get = function
    | [{txt = "len"; _}, PStr
         [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant (Pconst_integer(sz, _)); _}, _); _}]] ->
      Some (int_of_string sz)
    | _ ->
      None
  in
  let sz = match get fty.ptyp_attributes, get att with
  | Some sz, None
  | None, Some sz -> Some sz
  | Some _, Some _ -> loc_err loc "multiple field length attribute"
  | None, None -> None
  in
  let fty = match fty.ptyp_desc with
    | Ptyp_constr ({txt = Lident fty; _}, []) -> fty
    | _ ->
      loc_err fty.ptyp_loc "type identifier expected"
  in
  parse_field loc fname.txt fty sz

let cstruct decl =
  let {ptype_name = name; ptype_kind = kind;
       ptype_attributes = attrs; ptype_loc = loc; _} = decl in
  let fields = match kind with
    | Ptype_record fields -> List.map constr_field fields
    | _ -> loc_err loc "record type declaration expected"
  in
  let endian = match attrs with
    | [{txt = endian; _}, PStr []] -> endian
    | [_, _] -> loc_err loc "no attribute payload expected"
    | _ -> loc_err loc "too many attributes"
  in
  create_struct loc endian name fields

let cenum decl =
  let {ptype_name = name; ptype_kind = kind;
       ptype_attributes = attrs; ptype_loc = loc; _} = decl in
  let fields = match kind with
    | Ptype_variant fields -> fields
    | _ ->
      loc_err loc "expected variant type"
  in
  let width, sexp =
    match attrs with
    | ({txt = width; _}, PStr []) :: ({txt = "sexp"; _}, PStr []) :: [] ->
      width, true
    | ({txt = width; _}, PStr []) :: [] ->
      width, false
    | _ ->
      loc_err loc "invalid cenum attributes"
  in
  let n = ref Int64.minus_one in
  let incr_n () = n := Int64.succ !n in
  let fields = List.map constr_enum fields in
  let fields =
    List.map (function
        | (f, None)   -> incr_n (); (f, !n)
        | (f, Some i) -> n := i; (f, i)
      ) fields in
  name, fields, width, sexp

let signature_item' mapper = function
  | {psig_desc =
       Psig_extension (({txt = "cstruct"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     psig_loc = loc} ->
    output_struct_sig loc (cstruct decl)
  | {psig_desc =
       Psig_extension (({txt = "cenum"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     psig_loc = loc} ->
    let name, fields, width, sexp = cenum decl in
    output_enum_sig loc name fields width ~sexp
  | other ->
    [default_mapper.signature_item mapper other]

let signature mapper s =
  List.concat (List.map (signature_item' mapper) s)

let structure_item' mapper = function
  | {pstr_desc =
       Pstr_extension (({txt = "cstruct"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     pstr_loc = loc} ->
    output_struct loc (cstruct decl)
  | {pstr_desc =
       Pstr_extension (({txt = "cenum"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     pstr_loc = loc} ->
    let name, fields, width, sexp = cenum decl in
    output_enum loc name fields width ~sexp
  | other ->
    [default_mapper.structure_item mapper other]

let structure mapper s =
  List.concat (List.map (structure_item' mapper) s)

let () =
  Driver.register ~name:"ppx_cstruct" Versions.ocaml_404
    (fun _config _cookies -> {default_mapper with structure; signature})
