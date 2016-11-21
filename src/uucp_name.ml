(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

include Uucp_name_base

let name u =
  let u = Uchar.to_int u in
  match Uucp_tmap4bytes.get_uint16_pair Uucp_name_data.name_map u with
  | 0, 0 -> ""
  | p, 0 -> Printf.sprintf "%s%04X" Uucp_name_data.name_toks.(p) u
  | 0, s -> Uucp_name_data.name_toks.(s)
  | p, s ->
      Printf.sprintf "%s %s"
        Uucp_name_data.name_toks.(p) Uucp_name_data.name_toks.(s)

let name_alias u = Uucp_cmap.get Uucp_name_data.name_alias_map (Uchar.to_int u)

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
