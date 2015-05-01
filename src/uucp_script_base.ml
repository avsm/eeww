(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

type t =
  [ `Aghb
  | `Ahom
  | `Arab
  | `Armi
  | `Armn
  | `Avst
  | `Bali
  | `Bamu
  | `Bass
  | `Batk
  | `Beng
  | `Bopo
  | `Brah
  | `Brai
  | `Bugi
  | `Buhd
  | `Cakm
  | `Cans
  | `Cari
  | `Cham
  | `Cher
  | `Copt
  | `Cprt
  | `Cyrl
  | `Deva
  | `Dsrt
  | `Dupl
  | `Egyp
  | `Elba
  | `Ethi
  | `Geor
  | `Glag
  | `Goth
  | `Gran
  | `Grek
  | `Gujr
  | `Guru
  | `Hang
  | `Hani
  | `Hano
  | `Hatr
  | `Hebr
  | `Hira
  | `Hluw
  | `Hrkt
  | `Hmng
  | `Hung
  | `Ital
  | `Java
  | `Kali
  | `Kana
  | `Khar
  | `Khmr
  | `Khoj
  | `Knda
  | `Kthi
  | `Lana
  | `Laoo
  | `Latn
  | `Lepc
  | `Limb
  | `Lina
  | `Linb
  | `Lisu
  | `Lyci
  | `Lydi
  | `Mahj
  | `Mand
  | `Mani
  | `Mend
  | `Merc
  | `Mero
  | `Mlym
  | `Modi
  | `Mong
  | `Mroo
  | `Mtei
  | `Mult
  | `Mymr
  | `Narb
  | `Nbat
  | `Nkoo
  | `Ogam
  | `Olck
  | `Orkh
  | `Orya
  | `Osma
  | `Palm
  | `Pauc
  | `Perm
  | `Phag
  | `Phli
  | `Phlp
  | `Phnx
  | `Plrd
  | `Prti
  | `Qaai
  | `Rjng
  | `Runr
  | `Samr
  | `Sarb
  | `Saur
  | `Sgnw
  | `Shaw
  | `Shrd
  | `Sidd
  | `Sind
  | `Sinh
  | `Sora
  | `Sund
  | `Sylo
  | `Syrc
  | `Tagb
  | `Takr
  | `Tale
  | `Talu
  | `Taml
  | `Tavt
  | `Telu
  | `Tfng
  | `Tglg
  | `Thaa
  | `Thai
  | `Tibt
  | `Tirh
  | `Ugar
  | `Vaii
  | `Wara
  | `Xpeo
  | `Xsux
  | `Yiii
  | `Zinh
  | `Zyyy
  | `Zzzz ]

let pp ppf s = Format.fprintf ppf "%s" begin match s with
  | `Aghb -> "Aghb"
  | `Ahom -> "Ahom"
  | `Arab -> "Arab"
  | `Armi -> "Armi"
  | `Armn -> "Armn"
  | `Avst -> "Avst"
  | `Bali -> "Bali"
  | `Bamu -> "Bamu"
  | `Bass -> "Bass"
  | `Batk -> "Batk"
  | `Beng -> "Beng"
  | `Bopo -> "Bopo"
  | `Brah -> "Brah"
  | `Brai -> "Brai"
  | `Bugi -> "Bugi"
  | `Buhd -> "Buhd"
  | `Cakm -> "Cakm"
  | `Cans -> "Cans"
  | `Cari -> "Cari"
  | `Cham -> "Cham"
  | `Cher -> "Cher"
  | `Copt -> "Copt"
  | `Cprt -> "Cprt"
  | `Cyrl -> "Cyrl"
  | `Deva -> "Deva"
  | `Dsrt -> "Dsrt"
  | `Dupl -> "Dupl"
  | `Egyp -> "Egyp"
  | `Elba -> "Elba"
  | `Ethi -> "Ethi"
  | `Geor -> "Geor"
  | `Glag -> "Glag"
  | `Goth -> "Goth"
  | `Gran -> "Gran"
  | `Grek -> "Grek"
  | `Gujr -> "Gujr"
  | `Guru -> "Guru"
  | `Hang -> "Hang"
  | `Hani -> "Hani"
  | `Hano -> "Hano"
  | `Hatr -> "Hatr"
  | `Hebr -> "Hebr"
  | `Hira -> "Hira"
  | `Hluw -> "Hluw"
  | `Hmng -> "Hmng"
  | `Hrkt -> "Hrkt"
  | `Hung -> "Hung"
  | `Ital -> "Ital"
  | `Java -> "Java"
  | `Kali -> "Kali"
  | `Kana -> "Kana"
  | `Khar -> "Khar"
  | `Khmr -> "Khmr"
  | `Khoj -> "Khoj"
  | `Knda -> "Knda"
  | `Kthi -> "Kthi"
  | `Lana -> "Lana"
  | `Laoo -> "Laoo"
  | `Latn -> "Latn"
  | `Lepc -> "Lepc"
  | `Limb -> "Limb"
  | `Lina -> "Lina"
  | `Linb -> "Linb"
  | `Lisu -> "Lisu"
  | `Lyci -> "Lyci"
  | `Lydi -> "Lydi"
  | `Mahj -> "Mahj"
  | `Mand -> "Mand"
  | `Mani -> "Mani"
  | `Mend -> "Mend"
  | `Merc -> "Merc"
  | `Mero -> "Mero"
  | `Mlym -> "Mlym"
  | `Modi -> "Modi"
  | `Mong -> "Mong"
  | `Mroo -> "Mroo"
  | `Mtei -> "Mtei"
  | `Mult -> "Mult"
  | `Mymr -> "Mymr"
  | `Narb -> "Narb"
  | `Nbat -> "Nbat"
  | `Nkoo -> "Nkoo"
  | `Ogam -> "Ogam"
  | `Olck -> "Olck"
  | `Orkh -> "Orkh"
  | `Orya -> "Orya"
  | `Osma -> "Osma"
  | `Palm -> "Palm"
  | `Pauc -> "Pauc"
  | `Perm -> "Perm"
  | `Phag -> "Phag"
  | `Phli -> "Phli"
  | `Phlp -> "Phlp"
  | `Phnx -> "Phnx"
  | `Plrd -> "Plrd"
  | `Prti -> "Prti"
  | `Qaai -> "Qaai"
  | `Rjng -> "Rjng"
  | `Runr -> "Runr"
  | `Samr -> "Samr"
  | `Sarb -> "Sarb"
  | `Saur -> "Saur"
  | `Sgnw -> "Sgnw"
  | `Shaw -> "Shaw"
  | `Shrd -> "Shrd"
  | `Sidd -> "Sidd"
  | `Sind -> "Sind"
  | `Sinh -> "Sinh"
  | `Sora -> "Sora"
  | `Sund -> "Sund"
  | `Sylo -> "Sylo"
  | `Syrc -> "Syrc"
  | `Tagb -> "Tagb"
  | `Takr -> "Takr"
  | `Tale -> "Tale"
  | `Talu -> "Talu"
  | `Taml -> "Taml"
  | `Tavt -> "Tavt"
  | `Telu -> "Telu"
  | `Tfng -> "Tfng"
  | `Tglg -> "Tglg"
  | `Thaa -> "Thaa"
  | `Thai -> "Thai"
  | `Tibt -> "Tibt"
  | `Tirh -> "Tirh"
  | `Ugar -> "Ugar"
  | `Vaii -> "Vaii"
  | `Wara -> "Wara"
  | `Xpeo -> "Xpeo"
  | `Xsux -> "Xsux"
  | `Yiii -> "Yiii"
  | `Zinh -> "Zinh"
  | `Zyyy -> "Zyyy"
  | `Zzzz -> "Zzzz"
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
