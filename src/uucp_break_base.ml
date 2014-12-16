(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Line break *)

type line =
  [ `AI | `AL | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL | `CM | `CP
  | `CR | `EX | `GL | `H2 | `H3 | `HL | `HY | `ID | `IN | `IS | `JL
  | `JT | `JV | `LF | `NL | `NS | `NU | `OP | `PO | `PR | `QU | `RI
  | `SA | `SG | `SP | `SY | `WJ | `XX | `ZW ]

(* N.B. the order here matches the one of this table:
   http://www.unicode.org/reports/tr14/#Table2 *)

let line_of_byte : line array =
  [| `OP; `CL; `CP; `QU; `GL; `NS; `EX; `SY; `IS; `PR; `PO; `NU; `AL; `HL;
     `ID; `IN; `HY; `BA; `BB; `B2; `ZW; `CM; `WJ; `H2; `H3; `JL; `JV; `JT;
     `RI; `AI; `BK; `CB; `CJ; `CR; ` LF; `NL; `SA; `SG; `SP; `XX |]

let line_max = Array.length line_of_byte - 1

let line_to_byte = function
| `OP -> 0 | `CL -> 1 | `CP -> 2 | `QU -> 3 | `GL -> 4 | `NS -> 5 | `EX -> 6
| `SY -> 7 | `IS -> 8 | `PR -> 9 | `PO -> 10 | `NU -> 11 | `AL -> 12 | `HL -> 13
| `ID -> 14 | `IN -> 15 | `HY -> 16 | `BA -> 17 | `BB -> 18 | `B2 -> 19
| `ZW -> 20 | `CM -> 21 | `WJ -> 22 | `H2 -> 23 | `H3 -> 24 | `JL -> 25
| `JV -> 26 | `JT -> 27 | `RI -> 28 | `AI -> 29 | `BK -> 30 | `CB -> 31
| `CJ -> 32 | `CR -> 33 | `LF -> 34 | `NL -> 35 | `SA -> 36 | `SG -> 37
| `SP -> 38 | `XX -> 39

let pp_line ppf v = Format.fprintf ppf "%s" begin match v with
  | `AI -> "AI" | `AL -> "AL" | `B2 -> "B2" | `BA -> "BA" | `BB -> "BB"
  | `BK -> "BK" | `CB -> "CB" | `CJ -> "CJ" | `CL -> "CL" | `CM -> "CM"
  | `CP -> "CP" | `CR -> "CR" | `EX -> "EX" | `GL -> "GL" | `H2 -> "H2"
  | `H3 -> "H3" | `HL -> "HL" | `HY -> "HY" | `ID -> "ID" | `IN -> "IN"
  | `IS -> "IS" | `JL -> "JL" | `JT -> "JT" | `JV -> "JV" | `LF -> "LF"
  | `NL -> "NL" | `NS -> "NS" | `NU -> "NU" | `OP -> "OP" | `PO -> "PO"
  | `PR -> "PR" | `QU -> "QU" | `RI -> "RI" | `SA -> "SA" | `SG -> "SG"
  | `SP -> "SP" | `SY -> "SY" | `WJ -> "WJ" | `XX -> "XX" | `ZW -> "ZW"
  end

(* Grapheme cluster break *)

type grapheme_cluster =
  [ `CN | `CR | `EX | `L | `LF | `LV | `LVT | `PP | `RI | `SM | `T | `V | `XX ]

let grapheme_cluster_of_byte : grapheme_cluster array =
    [| `CN; `CR; `EX; `L; `LF; `LV; `LVT; `PP; `RI; `SM; `T; `V; `XX |]

let grapheme_cluster_max = Array.length grapheme_cluster_of_byte - 1

let grapheme_cluster_to_byte = function
| `CN -> 0 | `CR -> 1 | `EX -> 2 | `L -> 3 | `LF -> 4 | `LV -> 5 | `LVT -> 6
| `PP -> 7 | `RI -> 8 | `SM -> 9 | `T -> 10 | `V -> 11 | `XX -> 12

let pp_grapheme_cluster ppf v = Format.fprintf ppf "%s" begin match v with
  | `CN -> "CN" | `CR -> "CR" | `EX -> "EX" | `L -> "L" | `LF -> "LF"
  | `LV -> "LV" | `LVT -> "LVT" | `PP -> "PP" | `RI -> "RI" | `SM -> "SM"
  | `T -> "T" | `V -> "V" | `XX -> "XX"
  end

(* Word break *)

type word =
  [ `CR | `DQ | `EX | `Extend | `FO | `HL | `KA | `LE | `LF | `MB | `ML
  | `MN | `NL | `NU | `RI | `SQ | `XX ]

let word_of_byte : word array =
  [| `CR; `DQ; `EX; `Extend; `FO; `HL; `KA; `LE; `LF; `MB; `ML; `MN; `NL; `NU;
     `RI; `SQ; `XX |]

let word_max = Array.length word_of_byte - 1

let word_to_byte = function
| `CR -> 0 | `DQ -> 1 | `EX -> 2 | `Extend -> 3 | `FO -> 4 | `HL -> 5
| `KA -> 6 | `LE -> 7 | `LF -> 8 | `MB -> 9 | `ML -> 10 | `MN -> 11
| `NL -> 12 | `NU -> 13 | `RI -> 14 | `SQ -> 15 | `XX -> 16

let pp_word ppf v = Format.fprintf ppf "%s" begin match v with
  | `CR -> "CR" | `DQ -> "DQ" | `EX -> "EX" | `Extend -> "Extend"
  | `FO -> "FO" | `HL -> "HL" | `KA -> "KA" | `LE -> "LE" | `LF -> "LF"
  | `MB -> "MB" | `ML -> "ML" | `MN -> "MN" | `NL -> "NL" | `NU -> "NU"
  | `RI -> "RI" | `SQ -> "SQ" | `XX -> "XX"
  end

(* Sentence break *)

type sentence =
  [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP
  | `ST | `UP | `XX ]

let sentence_of_byte : sentence array =
  [| `AT; `CL; `CR; `EX; `FO; `LE; `LF; `LO; `NU; `SC; `SE; `SP; `ST; `UP; `XX|]

let sentence_max = Array.length sentence_of_byte - 1

let sentence_to_byte = function
| `AT -> 0 | `CL -> 1 | `CR -> 2 | `EX -> 3 | `FO -> 4 | `LE -> 5 | `LF -> 6
| `LO -> 7 | `NU -> 8 | `SC -> 9 | `SE -> 10 | `SP -> 11 | `ST -> 12
| `UP -> 13 | `XX -> 14

let pp_sentence ppf v = Format.fprintf ppf "%s" begin match v with
  | `AT -> "AT" | `CL -> "CL" | `CR -> "CR" | `EX -> "EX" | `FO -> "FO"
  | `LE -> "LE" | `LF -> "LF" | `LO -> "LO" | `NU -> "NU" | `SC -> "SC"
  | `SE -> "SE" | `SP -> "SP" | `ST -> "ST" | `UP -> "UP" | `XX -> "XX"
  end

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
