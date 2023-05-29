(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

module Sset = Asetmap.Set.Make (String)
module Smap = Asetmap.Map.Make_with_key_set (String) (Sset)

let m = Smap.(add "hey" 3 @@ add "bla" 2 @@ empty)
let s = Sset.(add "bla" @@ add "hey" @@ empty)

let test_dom () = assert (Sset.equal (Smap.dom m) s)
let test_set_to_list () = assert (Sset.to_list s = ["bla"; "hey"])
let test_map_to_list () = assert (Smap.to_list m = ["bla", 2; "hey", 3])

let () =
  test_dom ();
  test_set_to_list ();
  test_map_to_list ();
  Printf.printf "[OK] All tests succedded.\n%!";
  ()

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

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
