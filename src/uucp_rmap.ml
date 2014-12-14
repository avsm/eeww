(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Binary tree uchar ranges maps. *)

type 'a tree =
  | Empty
  | R of int * int * 'a
  | Rn of 'a tree * 'a tree * int * int * 'a

type 'a t = { default : 'a; tree : 'a tree }

let get m cp =
  let rec loop cp = function
  | Rn (l, r, is, ie, v) ->
      if cp < is then loop cp l else
      if cp > ie then loop cp r else
      v
  | R (is, ie, v) ->
      if cp < is then m.default else
      if cp > ie then m.default else
      v
  | Empty -> m.default
  in
  loop cp m.tree

let of_sorted_list default l =                           (* perfect balance. *)
  let rec loop len l =
    if len = 1 then match l with
    | `R (is, ie, v) :: r -> R (is, ie, v), r
    | _ -> assert false
    else
    let len_ll = len / 2 in
    let len_rl = len - len_ll in
    let ltree, rlist = loop len_ll l in
    match rlist with
    | [] -> ltree, []
    | `R (is, ie, v) :: r ->
        if len_rl = 1 then Rn (ltree, Empty, is, ie, v), r else
        let rtree, rlist = loop (len_rl - 1) r in
        Rn (ltree, rtree, is, ie, v), rlist
  in
  let keep acc (`R (_, _, v) as p) = if v <> default then p :: acc else acc in
  let l = List.rev (List.fold_left keep [] l) in
  let len = List.length l in
  let tree = if len = 0 then Empty else fst (loop len l) in
  { default; tree }

let height m =
  let rec loop = function
  | Empty -> 0
  | R _ -> 1
  | Rn (l, r, _, _, _) -> 1 + max (loop l) (loop r)
  in
  loop m.tree

let rec word_size v_size m =        (* value sharing not taken into account. *)
  let rec loop = function
  | Empty -> 0
  | R (_, _, v) -> 4 + v_size v
  | Rn (l, r, _, _, v) -> 6 + loop l + loop r + v_size v
  in
  loop m.tree

let rec dump pp_v ppf m =
  let pp = Format.fprintf in
  let rec dump_tree ppf = function
  | Rn (l, r, is, ie, v) ->
      pp ppf "@[<4>Rn(%a,@,%a,@,0x%04X,@,0x%04X,@,%a)@]"
        dump_tree l dump_tree r is ie pp_v v
  | R (is, ie, v) ->
      pp ppf "@[<3>R(0x%04X,@,0x%04X,@,%a)@]" is ie pp_v v
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
