(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
