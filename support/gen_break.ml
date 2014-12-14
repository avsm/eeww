(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp_line_break ppf ucd =
  let size v = 0 in
  let pp_line_break ppf v = Gen.pp ppf "`%a" Uucp_break_base.pp_line v in
  Gen.pp_prop_rmap_ucd ppf ucd
    Uucd.line_break "line_break" "Uucp_break_base.line"
    pp_line_break ~default:`XX  size

let pp_grapheme_cluster_break ppf ucd =
  let size v = 0 in
  let pp_grapheme_cluster_break ppf v =
    Gen.pp ppf "`%a" Uucp_break_base.pp_grapheme_cluster v
  in
  Gen.pp_prop_rmap_ucd ppf ucd
    Uucd.grapheme_cluster_break "grapheme_cluster_break"
    "Uucp_break_base.grapheme_cluster"
    pp_grapheme_cluster_break ~default:`XX  size

let pp_word_break ppf ucd =
  let size v = 0 in
  let pp_word_break ppf v = Gen.pp ppf "`%a" Uucp_break_base.pp_word v in
  Gen.pp_prop_rmap_ucd ppf ucd
    Uucd.word_break "word_break" "Uucp_break_base.word"
    pp_word_break ~default:`XX size

let pp_sentence_break ppf ucd =
  let size v = 0 in
  let pp_sentence_break ppf v =
    Gen.pp ppf "`%a" Uucp_break_base.pp_sentence v
  in
  Gen.pp_prop_rmap_ucd ppf ucd
    Uucd.sentence_break "sentence_break" "Uucp_break_base.sentence"
    pp_sentence_break ~default:`XX size

let pp_props ppf ucd =
  pp_line_break ppf ucd;
  pp_grapheme_cluster_break ppf ucd;
  pp_word_break ppf ucd;
  pp_sentence_break ppf ucd;
  ()

let pp_mod ppf ucd = Gen.pp_mod pp_props ppf ucd

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
