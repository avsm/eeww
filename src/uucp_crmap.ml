(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Binary tree uchar and uchar ranges maps. *)

type 'a tree =
  | Empty
  | C of int * 'a
  | R of int * int * 'a
  | Cn of 'a tree * 'a tree * int * 'a
  | Rn of 'a tree * 'a tree * int * int * 'a

type 'a t = { default : 'a; tree : 'a tree }

let get m cp =
  let rec loop cp = function
  | Rn (l, r, is, ie, v) ->
      if cp < is then loop cp l else
      if cp > ie then loop cp r else
      v
  | R (is, ie, v) ->
      if is <= cp && cp <= ie then v else m.default
  | Cn (l, r, i, v) ->
      if cp < i then loop cp l else
      if cp > i then loop cp r else
      v
  | C (i, v) ->
      if cp = i then v else m.default
  | Empty -> m.default
  in
  loop cp m.tree

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
