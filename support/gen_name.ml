(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* The name property being distinct for most scalar value it doesn't
   compress well using any of Uucp's maps, especially on 64-bit
   platforms, where a pointer costs 8 bytes. To give an idea having
   one pointer per scalar value already uses around 8 Mo of memory
   ((0x10_FFFF - 0x07FF) * 8 / (1024 * 1024)).

   For an efficient encoding of names we aribitrarily cut the name
   property of a scalar value in two at the last space character
   (U+0020). This results in two tokens that we index and to which we
   assign a unique code; this allows to share long common prefixes
   like "LATIN SMALL CAPITAL LETTER" among scalar values. We then use
   a trie that maps scalar values to 4 bytes in string chunks (see
   uucp_tmap4bytes.ml) and encode the two token codes as unsigned
   16-bit integers in these 4 bytes thereby avoiding having too many
   pointers.

   The lookup procedure simply gets the two 16-bit integers from the
   trie map, looks up the corresponding tokens in a table and
   concatenates them to form the name. We also keep the pattern naming
   mechanism inherited from the UCD XML for CJK names (see prop in
   pp_name below and Uucp_name.name).

   We tried a misguided effort to encode the tokens using a 6-bit
   encoding but this is pointless as the cumulated string size of the
   tokens is only about ~265Ko. So this buys only ~65Ko but at the
   cost of a significant increase in the complexity of the
   encoding. *)

let rcut c s =                                                  (* Again ... *)
  try
    let n = String.length s in
    let i = String.rindex s c in
    let left = String.sub s 0 i in
    let right =
      if i = n - 1 then "" else
      String.sub s (i + 1) (n - i - 1)
    in
    left, right
  with Not_found -> ("", s)

let name_prop ucd =
  let tok_index = Hashtbl.create 30_000 in
  let tok_code = ref (-1) in
  let get_tok_code tok = try Hashtbl.find tok_index tok with
  | Not_found -> incr tok_code; Hashtbl.add tok_index tok !tok_code; !tok_code
  in
  let prop u = match Gen.ucd_get ucd u Uucd.name with
  | `Name n ->
      begin match rcut ' ' n with
      | n, "" when n <> "" -> assert false
      | p, s -> (get_tok_code p), (get_tok_code s)
      end
  | `Pattern n ->
      begin match rcut '#' n with
      | n, "" -> (get_tok_code n), (get_tok_code "")
      | _ -> assert false
      end
  in
  ignore (get_tok_code ""); (* assign code 0 to "" *)
  let default = (0, 0) in
  tok_index, prop, Gen.prop_tmap4bytes_uint16_pair prop default

let pp_name ppf ucd =
  Gen.log "* name property, 4 bytes trie map and token table@\n";
  let tok_index, prop, (m, get) = name_prop ucd in
  let tok_list = Hashtbl.fold (fun t tc acc -> (tc, t) :: acc) tok_index [] in
  let tok_list = List.sort compare tok_list in
  let size = Uucp_tmap4bytes.word_size m in
  Gen.log "  size: %a" Gen.pp_size size;
  Gen.log " token count: %d@\n" (Hashtbl.length tok_index);
  Gen.log "  asserting"; Gen.assert_prop_map prop get;
  Gen.log ", generating@\n";
  Gen.pp ppf "@[let name_toks : string array = [|";
  List.iter (fun (_, t) -> Gen.pp ppf "%S;@," t) tok_list ;
  Gen.pp ppf "|]@]@\n";
  Gen.pp ppf "open Uucp_tmap4bytes@\n";
  Gen.pp ppf "@[let name_map : t = %a@]@\n" Uucp_tmap4bytes.dump m;
  ()

let pp_name_alias ppf ucd =
  let size v =
    3 * (List.length v) +
    List.fold_left (fun acc (_, n) -> 3 + String.length n) 0 v
  in
  let pp_tag ppf t = Gen.pp ppf "`%a" Uucp_name_base.pp_alias_tag t in
  let pp_alias ppf (t, n) = Gen.pp ppf "@[<1>(%a,@,%S)@]" pp_tag t n in
  let pp_alist = Gen.pp_list pp_alias in
  let prop u =
    let permute (n, t) = (t, n) in
    List.map permute (Gen.ucd_get ucd u Uucd.name_alias)
  in
  Gen.pp_prop_cmap ppf prop
    "name_alias" "(Uucp_name_base.alias_tag * string) list"
    pp_alist ~default:[] size

let pp_props ppf ucd =
  pp_name ppf ucd;
  pp_name_alias ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
