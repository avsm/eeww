(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Printf.sprintf
let err_no_pred u = str "U+%04X has no predecessor" u
let err_no_succ u = str "U+%04X has no sucessor" u
let err_not_sv i = str "%X is not an Unicode scalar value" i

(* Characters *)

type t = int

let min = 0x0000
let max = 0x10FFFF
let lo_bound = 0xD7FF
let hi_bound = 0xE000
let is_uchar i = (min <= i && i <= lo_bound) || (hi_bound <= i && i <= max)

let succ u =
  if u = lo_bound then hi_bound else
  if u = max then invalid_arg (err_no_succ u) else
  u + 1

let pred u =
  if u = hi_bound then lo_bound else
  if u = min then invalid_arg (err_no_pred u) else
  u - 1

let of_int i = if is_uchar i then i else invalid_arg (err_not_sv i)
external unsafe_of_int : t -> int = "%identity"
external to_int : t -> int = "%identity"
let equal : int -> int -> bool = ( = )
let compare : int -> int -> int = Pervasives.compare

(* Full Unicode chracter set traversal *)

let iter f =
  for u = min to lo_bound do f u done;
  for u = hi_bound to max do f u done

let fold f acc =
  let rec loop f acc u =
    if u > max then acc else
    if u = lo_bound then loop f (f acc u) hi_bound else
    loop f (f acc u) (u + 1)
  in
  loop f acc min

(* Printers *)

let pp ppf u = Format.fprintf ppf "U+%04X" u

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
