(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

type line =
  [ `AI | `AL | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL | `CM | `CP
  | `CR | `EX | `GL | `H2 | `H3 | `HL | `HY | `ID | `IN | `IS | `JL
  | `JT | `JV | `LF | `NL | `NS | `NU | `OP | `PO | `PR | `QU | `RI
  | `SA | `SG | `SP | `SY | `WJ | `XX | `ZW ]

let byte_to_line : line array =
  [| `AI ; `AL ; `B2 ; `BA ; `BB ; `BK ; `CB ; `CJ ; `CL ; `CM ; `CP;
     `CR ; `EX ; `GL ; `H2 ; `H3 ; `HL ; `HY ; `ID ; `IN ; `IS ; `JL;
     `JT ; `JV ; `LF ; `NL ; `NS ; `NU ; `OP ; `PO ; `PR ; `QU ; `RI;
     `SA ; `SG ; `SP ; `SY ; `WJ ; `XX ; `ZW |]

type grapheme_cluster =
  [ `CN | `CR | `EX | `L | `LF | `LV | `LVT | `PP | `RI | `SM | `T | `V | `XX ]

type word =
  [ `CR | `DQ | `EX | `Extend | `FO | `HL | `KA | `LE | `LF | `MB | `ML
  | `MN | `NL | `NU | `RI | `SQ | `XX ]

type sentence =
  [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP
  | `ST | `UP | `XX ]

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

let pp_grapheme_cluster ppf v = Format.fprintf ppf "%s" begin match v with
  | `CN -> "CN" | `CR -> "CR" | `EX -> "EX" | `L -> "L" | `LF -> "LF"
  | `LV -> "LV" | `LVT -> "LVT" | `PP -> "PP" | `RI -> "RI" | `SM -> "SM"
  | `T -> "T" | `V -> "V" | `XX -> "XX"
  end

let pp_word ppf v = Format.fprintf ppf "%s" begin match v with
  | `CR -> "CR" | `DQ -> "DQ" | `EX -> "EX" | `Extend -> "Extend"
  | `FO -> "FO" | `HL -> "HL" | `KA -> "KA" | `LE -> "LE" | `LF -> "LF"
  | `MB -> "MB" | `ML -> "ML" | `MN -> "MN" | `NL -> "NL" | `NU -> "NU"
  | `RI -> "RI" | `SQ -> "SQ" | `XX -> "XX"
  end

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
