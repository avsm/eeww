(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

let pp_line_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.line_to_byte Uucd.line_break "line_break" ~default:`XX
    Uucp_break_base.pp_line

let pp_grapheme_cluster_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.grapheme_cluster_to_byte Uucd.grapheme_cluster_break
    "grapheme_cluster_break" ~default:`XX Uucp_break_base.pp_grapheme_cluster

let pp_word_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.word_to_byte Uucd.word_break
    "word_break" ~default:`XX Uucp_break_base.pp_word

let pp_sentence_break ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.sentence_to_byte Uucd.sentence_break
    "sentence_break" ~default:`XX Uucp_break_base.pp_sentence

let pp_east_asian_width ppf ucd =
  Gen.pp_code_prop_tmapbyte_ucd ppf ucd
    Uucp_break_base.east_asian_width_to_byte Uucd.east_asian_width
    "east_asian_width" ~default:`N Uucp_break_base.pp_east_asian_width

let pp_props ppf ucd =
  pp_line_break ppf ucd;
  pp_grapheme_cluster_break ppf ucd;
  pp_word_break ppf ucd;
  pp_sentence_break ppf ucd;
  pp_east_asian_width ppf ucd;
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
