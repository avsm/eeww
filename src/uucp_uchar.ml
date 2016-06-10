(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
