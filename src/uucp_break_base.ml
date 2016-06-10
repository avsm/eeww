(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Line break *)

type line =
  [ `AI | `AL | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL | `CM | `CP
  | `CR | `EX | `GL | `H2 | `H3 | `HL | `HY | `ID | `IN | `IS | `JL
  | `JT | `JV | `LF | `NL | `NS | `NU | `OP | `PO | `PR | `QU | `RI
  | `SA | `SG | `SP | `SY | `WJ | `XX | `ZW ]

let line_of_byte : line array =
  [| `AI; `AL; `B2; `BA; `BB; `BK; `CB; `CJ; `CL; `CM; `CP; `CR; `EX; `GL; `H2;
     `H3; `HL; `HY; `ID; `IN; `IS; `JL; `JT; `JV; `LF; `NL; `NS; `NU; `OP; `PO;
     `PR; `QU; `RI; `SA; `SG; `SP; `SY; `WJ; `XX; `ZW |]

let line_max = Array.length line_of_byte - 1

let line_to_byte = function
| `AI -> 0 | `AL -> 1 | `B2 -> 2 | `BA -> 3 | `BB -> 4 | `BK -> 5 | `CB -> 6
| `CJ -> 7 | `CL -> 8 | `CM -> 9 | `CP -> 10 | `CR -> 11 | `EX -> 12
| `GL -> 13| `H2 -> 14 | `H3 -> 15 | `HL -> 16 | `HY -> 17 | `ID -> 18
| `IN -> 19 | `IS -> 20 | `JL -> 21 | `JT -> 22 | `JV -> 23 | `LF -> 24
| `NL -> 25 | `NS -> 26 | `NU -> 27 | `OP -> 28 | `PO -> 29 | `PR -> 30
| `QU -> 31 | `RI -> 32 | `SA -> 33 | `SG -> 34 | `SP -> 35 | `SY -> 36
| `WJ -> 37 | `XX -> 38 | `ZW -> 39

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

(* East Asian width *)

type east_asian_width = [ `A | `F | `H | `N | `Na | `W ]
let pp_east_asian_width ppf v = Format.pp_print_string ppf begin match v with
| `A -> "A" | `F -> "F" | `H -> "H" | `N -> "N" | `Na -> "Na" | `W -> "W"
end

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
