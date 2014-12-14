(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Binary tree uchar maps. *)

type 'a tree =
  | Empty
  | C of int * 'a
  | Cn of 'a tree * 'a tree * int * 'a

type 'a t = { default : 'a; tree : 'a tree }

let get m cp =
  let rec loop cp = function
  | Cn (l, r, i, v) ->
      if cp < i then loop cp l else
      if cp > i then loop cp r else
      v
  | C (i, v) -> if cp = i then v else m.default
  | Empty -> m.default
  in
  loop cp m.tree

let of_sorted_list default l =                           (* perfect balance. *)
  let rec loop len l =
    if len = 1 then match l with
    | `C (i, v) :: r -> C (i, v), r
    | _ -> assert false
    else
    let len_ll = len / 2 in
    let len_rl = len - len_ll in
    let ltree, rlist = loop len_ll l in
    match rlist with
    | [] -> ltree, []
    | `C (i, v) :: r ->
        if len_rl = 1 then Cn (ltree, Empty, i, v), r else
        let rtree, rlist = loop (len_rl - 1) r in
        Cn (ltree, rtree, i, v), rlist
  in
  let keep acc (`C (_, v) as p) = if v <> default then p :: acc else acc in
  let l = List.rev (List.fold_left keep [] l) in
  let len = List.length l in
  let tree = if len = 0 then Empty else fst (loop len l) in
  { default; tree }

let height m =
  let rec loop = function
  | Empty -> 0
  | C _ -> 1
  | Cn (l, r, _, _) -> 1 + max (loop l) (loop r)
  in
  loop m.tree

let word_size v_size m =            (* value sharing not taken into account. *)
  let rec loop = function
  | Empty -> 0
  | C (_, v) -> 3 + v_size v
  | Cn (l, r, _, v) -> 5 + loop l + loop r + v_size v
  in
  loop m.tree

let rec dump pp_v ppf m =
  let pp = Format.fprintf in
  let rec dump_tree ppf = function
  | Cn (l, r, i, v) ->
      pp ppf "@[<4>Cn(%a,@,%a,@,0x%04X,@,%a)@]" dump_tree l dump_tree r i pp_v v
  | C (i, v) ->
      pp ppf "@[<3>C(0x%04X,@,%a)@]" i pp_v v
  | Empty ->
      pp ppf "Empty"
  in
  pp ppf "@,{ default =@ %a;@, tree =@ " pp_v m.default;
  dump_tree ppf m.tree;
  pp ppf "@,}"


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
