(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let log fmt = Format.eprintf (fmt ^^ "%!")
let pp = Format.fprintf
let str = Format.asprintf
let pp_size ppf s =
  let b = s * (Sys.word_size / 8) in
  if b < 1_048_576 then pp ppf "%.1f Ko" (float b /. 1024.) else
  if b < 1_073_741_824 then pp ppf "%.1f Mo" (float b /. (1024. ** 2.)) else
  pp ppf "%.1f Go" (float b /. (1024. ** 3.))

let pp_list pp_v ppf vs =
  let rec loop = function
  | v :: vs -> pp ppf "@[%a@];@," pp_v v; loop vs
  | [] -> ()
  in
  pp ppf "@[<1>["; loop vs; pp ppf "]@]"

(* Property lookup *)

let ucd_find ucd u p = Uucd.cp_prop ucd u p
let ucd_get ucd u p = match Uucd.cp_prop ucd u p with
| None -> invalid_arg (str "miss property for U+%04X in character database" u)
| Some v -> v

(* Generic map builders *)

let assert_prop_map prop get =
  let assert_u u =
    if prop u = get u then () else
    failwith (str "map failure for U+%04X" u)
  in
  Uucp_uchar.iter assert_u

let prop_map create set get prop default =
  let m = create default in
  let add_u u = set m u (prop u) in
  Uucp_uchar.iter add_u; m, (get m)

(* Generate Uucp_cmap.t values *)

let prop_cmap ~default prop =
  let m = ref [] in
  let add_u u = m := (`C (u, prop u)) :: !m in
  Uucp_uchar.iter add_u; Uucp_cmap.of_sorted_list default (List.rev !m)

let pp_prop_cmap ppf prop pname ptype pp_prop ~default size_v =
  log "* %s property, binary tree character map@\n" pname;
  let m = prop_cmap ~default prop in
  let size = Uucp_cmap.word_size size_v m in
  let h = Uucp_cmap.height m in
  log "  size (default %a): %a height: %d@\n" pp_prop default pp_size size h;
  log "  asserting"; assert_prop_map prop (Uucp_cmap.get m);
  log ", generating@\n";
  pp ppf "open Uucp_cmap@\n";
  pp ppf "@[let %s_map : %s t = %a@]@\n" pname ptype (Uucp_cmap.dump pp_prop) m;
  ()

let pp_prop_cmap_ucd ppf ucd prop pname ptype pp_prop ~default size_v =
  let prop u = ucd_get ucd u prop in
  pp_prop_cmap ppf prop pname ptype pp_prop ~default size_v

(* Generate Uucp_rmap.t value *)

let prop_find_ranges prop =
  let current = ref None in
  let start = ref 0 in
  let ranges = ref [] in
  let rec add_u u =
    let p = prop u in
    let add_range v max =
      ranges := (`R (!start, max, v)) :: !ranges;
      current := None; add_u u
    in
    match !current with
    | None -> current := Some p; start := u
    | Some v ->
        if v = p then (if u = Uucp_uchar.max then add_range v u) else
        add_range v (u - 1)
  in
  Uucp_uchar.iter add_u; (List.rev !ranges)

let pp_prop_rmap ppf prop pname ptype pp_prop ~default size_v =
  log "* %s property, binary tree range code point map@\n" pname;
  let m = Uucp_rmap.of_sorted_list default (prop_find_ranges prop) in
  let size = Uucp_rmap.word_size size_v m in
  let h = Uucp_rmap.height m in
  log "  size (default %a): %a height: %d@\n" pp_prop default pp_size size h;
  log "  asserting"; assert_prop_map prop (Uucp_rmap.get m);
  log ", generating@\n";
  pp ppf "open Uucp_rmap@\n";
  pp ppf "@[let %s_map : %s t = %a@]@\n" pname ptype (Uucp_rmap.dump pp_prop) m;
  ()

let pp_prop_rmap_ucd ppf ucd prop pname ptype pp_prop ~default size_v =
  let prop u = ucd_get ucd u prop in
  pp_prop_rmap ppf prop pname ptype pp_prop ~default size_v

(* Generate Uucp_tmap.t values *)

let prop_tmap prop default =
  prop_map
    Uucp_tmap.create
    Uucp_tmap.set
    Uucp_tmap.get
    prop default

let pp_prop_tmap ppf prop pname ptype pp_prop ~default size_v =
  log "* %s property, trie map@\n" pname;
  let m, get = prop_tmap prop default in
  let t_size = Uucp_tmap.word_size size_v m in
  log "  size (default %a): %a@\n" pp_prop default pp_size t_size;
  log "  asserting"; assert_prop_map prop get;
  log ", generating@\n";
  pp ppf "open Uucp_tmap@\n";
  pp ppf "@[let %s_map : %s t = %a@]@\n" pname ptype (Uucp_tmap.dump pp_prop) m;
  ()

let pp_prop_tmap_ucd ppf ucd prop pname ptype pp_prop ~default size_v =
  let prop u = ucd_get ucd u prop in
  pp_prop_tmap ppf prop pname ptype pp_prop ~default size_v

(* Generate Uucp_tmapbool.t value *)

let prop_tmapbools prop =
  let tm = Uucp_tmapbool.create true in
  let fm = Uucp_tmapbool.create false in
  let add_u u =
    let b = prop u in
    Uucp_tmapbool.set tm u b;
    Uucp_tmapbool.set fm u b;
  in
  Uucp_uchar.iter add_u; tm, fm

let assert_tmapbools prop tm fm =
  let assert_u u =
    let fail () = failwith (str "bool prop map failure for U+%04X" u) in
    let b = prop u in
    if b <> Uucp_tmapbool.get tm u then fail ();
    if b <> Uucp_tmapbool.get fm u then fail ();
  in
  Uucp_uchar.iter assert_u

let pp_prop_tmapbool ppf prop pname =
  log "* %s property, boolean trie map@\n" pname;
  let tm, fm = prop_tmapbools prop in
  let tm_size, fm_size =
    Uucp_tmapbool.word_size tm, Uucp_tmapbool.word_size fm
  in
  let use_fm = tm_size > fm_size in
  log "  size (default true): %a, size (default false): %a@\n"
    pp_size tm_size pp_size fm_size;
  log "  using default %b map"  (not use_fm);
  log ", asserting"; assert_tmapbools prop tm fm;
  log ", generating@\n";
  let m = if use_fm then fm else tm in
  pp ppf "open Uucp_tmapbool@\n";
  pp ppf "@[let %s_map = %a@]@\n" pname Uucp_tmapbool.dump m;
  ()

let pp_prop_tmapbool_ucd ppf ucd prop pname =
  let prop u = ucd_get ucd u prop in
  pp_prop_tmapbool ppf prop pname

(* Genenerate Uucp_tmapbyte.t values *)

let prop_tmapbyte prop default =
  prop_map
    Uucp_tmapbyte.create
    Uucp_tmapbyte.set
    Uucp_tmapbyte.get
    prop default

let pp_prop_tmapbyte ppf prop pname ~default =
  log "* %s property, trie byte map@\n" pname;
  let m, get = prop_tmapbyte prop default in
  let size = Uucp_tmapbyte.word_size m in
  log " size (default %d): %a@\n" default pp_size size;
  log " asserting"; assert_prop_map prop get;
  log ", generating@\n";
  pp ppf "open Uucp_tmapbyte@\n";
  pp ppf "@[let %s_map : t = %a@]@\n" pname Uucp_tmapbyte.dump m;
  ()

let pp_prop_tmapbyte_ucd ppf ucd prop pname ~default =
  let prop u = ucd_get ucd u prop in
  pp_prop_tmapbyte ppf prop pname ~default

(* Generate Uucp_tmap4bytes.t values. *)

let prop_tmap4bytes_uint16_pair prop default =
  prop_map
    Uucp_tmap4bytes.create_uint16_pair
    Uucp_tmap4bytes.set_uint16_pair
    Uucp_tmap4bytes.get_uint16_pair
    prop default

(* Generate a module *)

let pp_mod pp_mod ppf m =
  pp ppf
"\
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* WARNING do not edit. This file was automatically generated. *)
@\n@[%a@]@\n
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
" pp_mod m

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
