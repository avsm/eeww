(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

type t =
  [ `ASCII
  | `Aegean_Numbers
  | `Ahom
  | `Alchemical
  | `Alphabetic_PF
  | `Anatolian_Hieroglyphs
  | `Ancient_Greek_Music
  | `Ancient_Greek_Numbers
  | `Ancient_Symbols
  | `Arabic
  | `Arabic_Ext_A
  | `Arabic_Math
  | `Arabic_PF_A
  | `Arabic_PF_B
  | `Arabic_Sup
  | `Armenian
  | `Arrows
  | `Avestan
  | `Balinese
  | `Bamum
  | `Bamum_Sup
  | `Bassa_Vah
  | `Batak
  | `Bengali
  | `Block_Elements
  | `Bopomofo
  | `Bopomofo_Ext
  | `Box_Drawing
  | `Brahmi
  | `Braille
  | `Buginese
  | `Buhid
  | `Byzantine_Music
  | `CJK
  | `CJK_Compat
  | `CJK_Compat_Forms
  | `CJK_Compat_Ideographs
  | `CJK_Compat_Ideographs_Sup
  | `CJK_Ext_A
  | `CJK_Ext_B
  | `CJK_Ext_C
  | `CJK_Ext_D
  | `CJK_Ext_E
  | `CJK_Radicals_Sup
  | `CJK_Strokes
  | `CJK_Symbols
  | `Carian
  | `Caucasian_Albanian
  | `Chakma
  | `Cham
  | `Cherokee
  | `Cherokee_Sup
  | `Compat_Jamo
  | `Control_Pictures
  | `Coptic
  | `Coptic_Epact_Numbers
  | `Counting_Rod
  | `Cuneiform
  | `Cuneiform_Numbers
  | `Currency_Symbols
  | `Cypriot_Syllabary
  | `Cyrillic
  | `Cyrillic_Ext_A
  | `Cyrillic_Ext_B
  | `Cyrillic_Sup
  | `Deseret
  | `Devanagari
  | `Devanagari_Ext
  | `Diacriticals
  | `Diacriticals_Ext
  | `Diacriticals_For_Symbols
  | `Diacriticals_Sup
  | `Dingbats
  | `Domino
  | `Duployan
  | `Early_Dynastic_Cuneiform
  | `Egyptian_Hieroglyphs
  | `Elbasan
  | `Emoticons
  | `Enclosed_Alphanum
  | `Enclosed_Alphanum_Sup
  | `Enclosed_CJK
  | `Enclosed_Ideographic_Sup
  | `Ethiopic
  | `Ethiopic_Ext
  | `Ethiopic_Ext_A
  | `Ethiopic_Sup
  | `Geometric_Shapes
  | `Geometric_Shapes_Ext
  | `Georgian
  | `Georgian_Sup
  | `Glagolitic
  | `Gothic
  | `Grantha
  | `Greek
  | `Greek_Ext
  | `Gujarati
  | `Gurmukhi
  | `Half_And_Full_Forms
  | `Half_Marks
  | `Hangul
  | `Hanunoo
  | `Hatran
  | `Hebrew
  | `Hiragana
  | `IDC
  | `IPA_Ext
  | `Imperial_Aramaic
  | `Indic_Number_Forms
  | `Inscriptional_Pahlavi
  | `Inscriptional_Parthian
  | `Jamo
  | `Jamo_Ext_A
  | `Jamo_Ext_B
  | `Javanese
  | `Kaithi
  | `Kana_Sup
  | `Kanbun
  | `Kangxi
  | `Kannada
  | `Katakana
  | `Katakana_Ext
  | `Kayah_Li
  | `Kharoshthi
  | `Khmer
  | `Khmer_Symbols
  | `Khojki
  | `Khudawadi
  | `Lao
  | `Latin_1_Sup
  | `Latin_Ext_A
  | `Latin_Ext_Additional
  | `Latin_Ext_B
  | `Latin_Ext_C
  | `Latin_Ext_D
  | `Latin_Ext_E
  | `Lepcha
  | `Letterlike_Symbols
  | `Limbu
  | `Linear_A
  | `Linear_B_Ideograms
  | `Linear_B_Syllabary
  | `Lisu
  | `Lycian
  | `Lydian
  | `Mahajani
  | `Mahjong
  | `Malayalam
  | `Mandaic
  | `Manichaean
  | `Math_Alphanum
  | `Math_Operators
  | `Meetei_Mayek
  | `Meetei_Mayek_Ext
  | `Mende_Kikakui
  | `Meroitic_Cursive
  | `Meroitic_Hieroglyphs
  | `Miao
  | `Misc_Arrows
  | `Misc_Math_Symbols_A
  | `Misc_Math_Symbols_B
  | `Misc_Pictographs
  | `Misc_Symbols
  | `Misc_Technical
  | `Modi
  | `Modifier_Letters
  | `Modifier_Tone_Letters
  | `Mongolian
  | `Mro
  | `Music
  | `Multani
  | `Myanmar
  | `Myanmar_Ext_A
  | `Myanmar_Ext_B
  | `NB
  | `NKo
  | `Nabataean
  | `New_Tai_Lue
  | `Number_Forms
  | `OCR
  | `Ogham
  | `Ol_Chiki
  | `Old_Hungarian
  | `Old_Italic
  | `Old_North_Arabian
  | `Old_Permic
  | `Old_Persian
  | `Old_South_Arabian
  | `Old_Turkic
  | `Oriya
  | `Ornamental_Dingbats
  | `Osmanya
  | `PUA
  | `Pahawh_Hmong
  | `Palmyrene
  | `Pau_Cin_Hau
  | `Phags_Pa
  | `Phaistos
  | `Phoenician
  | `Phonetic_Ext
  | `Phonetic_Ext_Sup
  | `Playing_Cards
  | `Psalter_Pahlavi
  | `Punctuation
  | `Rejang
  | `Rumi
  | `Runic
  | `Samaritan
  | `Saurashtra
  | `Sharada
  | `Shavian
  | `Shorthand_Format_Controls
  | `Siddham
  | `Sinhala
  | `Sinhala_Archaic_Numbers
  | `Small_Forms
  | `Sora_Sompeng
  | `Specials
  | `Sundanese
  | `Sundanese_Sup
  | `Sup_Arrows_A
  | `Sup_Arrows_B
  | `Sup_Arrows_C
  | `Sup_Math_Operators
  | `Sup_PUA_A
  | `Sup_PUA_B
  | `Sup_Punctuation
  | `Sup_Symbols_And_Pictographs
  | `Super_And_Sub
  | `Sutton_SignWriting
  | `Syloti_Nagri
  | `Syriac
  | `Tagalog
  | `Tagbanwa
  | `Tags
  | `Tai_Le
  | `Tai_Tham
  | `Tai_Viet
  | `Tai_Xuan_Jing
  | `Takri
  | `Tamil
  | `Telugu
  | `Thaana
  | `Thai
  | `Tibetan
  | `Tifinagh
  | `Tirhuta
  | `Transport_And_Map
  | `UCAS
  | `UCAS_Ext
  | `Ugaritic
  | `VS
  | `VS_Sup
  | `Vai
  | `Vedic_Ext
  | `Vertical_Forms
  | `Warang_Citi
  | `Yi_Radicals
  | `Yi_Syllables
  | `Yijing ]

let pp ppf b = Format.fprintf ppf "%s" begin match b with
  | `ASCII -> "ASCII"
  | `Aegean_Numbers -> "Aegean_Numbers"
  | `Ahom -> "Ahom"
  | `Alchemical -> "Alchemical"
  | `Alphabetic_PF -> "Alphabetic_PF"
  | `Anatolian_Hieroglyphs -> "Anatolian_Hieroglyphs"
  | `Ancient_Greek_Music -> "Ancient_Greek_Music"
  | `Ancient_Greek_Numbers -> "Ancient_Greek_Numbers"
  | `Ancient_Symbols -> "Ancient_Symbols"
  | `Arabic -> "Arabic"
  | `Arabic_Ext_A -> "Arabic_Ext_A"
  | `Arabic_Math -> "Arabic_Math"
  | `Arabic_PF_A -> "Arabic_PF_A"
  | `Arabic_PF_B -> "Arabic_PF_B"
  | `Arabic_Sup -> "Arabic_Sup"
  | `Armenian -> "Armenian"
  | `Arrows -> "Arrows"
  | `Avestan -> "Avestan"
  | `Balinese -> "Balinese"
  | `Bamum -> "Bamum"
  | `Bamum_Sup -> "Bamum_Sup"
  | `Bassa_Vah -> "Bassa_Vah"
  | `Batak -> "Batak"
  | `Bengali -> "Bengali"
  | `Block_Elements -> "Block_Elements"
  | `Bopomofo -> "Bopomofo"
  | `Bopomofo_Ext -> "Bopomofo_Ext"
  | `Box_Drawing -> "Box_Drawing"
  | `Brahmi -> "Brahmi"
  | `Braille -> "Braille"
  | `Buginese -> "Buginese"
  | `Buhid -> "Buhid"
  | `Byzantine_Music -> "Byzantine_Music"
  | `CJK -> "CJK"
  | `CJK_Compat -> "CJK_Compat"
  | `CJK_Compat_Forms -> "CJK_Compat_Forms"
  | `CJK_Compat_Ideographs -> "CJK_Compat_Ideographs"
  | `CJK_Compat_Ideographs_Sup -> "CJK_Compat_Ideographs_Sup"
  | `CJK_Ext_A -> "CJK_Ext_A"
  | `CJK_Ext_B -> "CJK_Ext_B"
  | `CJK_Ext_C -> "CJK_Ext_C"
  | `CJK_Ext_D -> "CJK_Ext_D"
  | `CJK_Ext_E -> "CJK_Ext_E"
  | `CJK_Radicals_Sup -> "CJK_Radicals_Sup"
  | `CJK_Strokes -> "CJK_Strokes"
  | `CJK_Symbols -> "CJK_Symbols"
  | `Carian -> "Carian"
  | `Caucasian_Albanian -> "Caucasian_Albanian"
  | `Chakma -> "Chakma"
  | `Cham -> "Cham"
  | `Cherokee -> "Cherokee"
  | `Cherokee_Sup -> "Cherokee_Sup"
  | `Compat_Jamo -> "Compat_Jamo"
  | `Control_Pictures -> "Control_Pictures"
  | `Coptic -> "Coptic"
  | `Coptic_Epact_Numbers -> "Coptic_Epact_Numbers"
  | `Counting_Rod -> "Counting_Rod"
  | `Cuneiform -> "Cuneiform"
  | `Cuneiform_Numbers -> "Cuneiform_Numbers"
  | `Currency_Symbols -> "Currency_Symbols"
  | `Cypriot_Syllabary -> "Cypriot_Syllabary"
  | `Cyrillic -> "Cyrillic"
  | `Cyrillic_Ext_A -> "Cyrillic_Ext_A"
  | `Cyrillic_Ext_B -> "Cyrillic_Ext_B"
  | `Cyrillic_Sup -> "Cyrillic_Sup"
  | `Deseret -> "Deseret"
  | `Devanagari -> "Devanagari"
  | `Devanagari_Ext -> "Devanagari_Ext"
  | `Diacriticals -> "Diacriticals"
  | `Diacriticals_Ext -> "Diacriticals_Ext"
  | `Diacriticals_For_Symbols -> "Diacriticals_For_Symbols"
  | `Diacriticals_Sup -> "Diacriticals_Sup"
  | `Dingbats -> "Dingbats"
  | `Domino -> "Domino"
  | `Duployan -> "Duployan"
  | `Early_Dynastic_Cuneiform -> "Early_Dynastic_Cuneiform"
  | `Egyptian_Hieroglyphs -> "Egyptian_Hieroglyphs"
  | `Elbasan -> "Elbasan"
  | `Emoticons -> "Emoticons"
  | `Enclosed_Alphanum -> "Enclosed_Alphanum"
  | `Enclosed_Alphanum_Sup -> "Enclosed_Alphanum_Sup"
  | `Enclosed_CJK -> "Enclosed_CJK"
  | `Enclosed_Ideographic_Sup -> "Enclosed_Ideographic_Sup"
  | `Ethiopic -> "Ethiopic"
  | `Ethiopic_Ext -> "Ethiopic_Ext"
  | `Ethiopic_Ext_A -> "Ethiopic_Ext_A"
  | `Ethiopic_Sup -> "Ethiopic_Sup"
  | `Geometric_Shapes -> "Geometric_Shapes"
  | `Geometric_Shapes_Ext -> "Geometric_Shapes_Ext"
  | `Georgian -> "Georgian"
  | `Georgian_Sup -> "Georgian_Sup"
  | `Glagolitic -> "Glagolitic"
  | `Gothic -> "Gothic"
  | `Grantha -> "Grantha"
  | `Greek -> "Greek"
  | `Greek_Ext -> "Greek_Ext"
  | `Gujarati -> "Gujarati"
  | `Gurmukhi -> "Gurmukhi"
  | `Half_And_Full_Forms -> "Half_And_Full_Forms"
  | `Half_Marks -> "Half_Marks"
  | `Hangul -> "Hangul"
  | `Hanunoo -> "Hanunoo"
  | `Hatran -> "Hatran"
  | `Hebrew -> "Hebrew"
  | `High_PU_Surrogates -> "High_PU_Surrogates"
  | `High_Surrogates -> "High_Surrogates"
  | `Hiragana -> "Hiragana"
  | `IDC -> "IDC"
  | `IPA_Ext -> "IPA_Ext"
  | `Imperial_Aramaic -> "Imperial_Aramaic"
  | `Indic_Number_Forms -> "Indic_Number_Forms"
  | `Inscriptional_Pahlavi -> "Inscriptional_Pahlavi"
  | `Inscriptional_Parthian -> "Inscriptional_Parthian"
  | `Jamo -> "Jamo"
  | `Jamo_Ext_A -> "Jamo_Ext_A"
  | `Jamo_Ext_B -> "Jamo_Ext_B"
  | `Javanese -> "Javanese"
  | `Kaithi -> "Kaithi"
  | `Kana_Sup -> "Kana_Sup"
  | `Kanbun -> "Kanbun"
  | `Kangxi -> "Kangxi"
  | `Kannada -> "Kannada"
  | `Katakana -> "Katakana"
  | `Katakana_Ext -> "Katakana_Ext"
  | `Kayah_Li -> "Kayah_Li"
  | `Kharoshthi -> "Kharoshthi"
  | `Khmer -> "Khmer"
  | `Khmer_Symbols -> "Khmer_Symbols"
  | `Khojki -> "Khojki"
  | `Khudawadi -> "Khudawadi"
  | `Lao -> "Lao"
  | `Latin_1_Sup -> "Latin_1_Sup"
  | `Latin_Ext_A -> "Latin_Ext_A"
  | `Latin_Ext_Additional -> "Latin_Ext_Additional"
  | `Latin_Ext_B -> "Latin_Ext_B"
  | `Latin_Ext_C -> "Latin_Ext_C"
  | `Latin_Ext_D -> "Latin_Ext_D"
  | `Latin_Ext_E -> "Latin_Ext_E"
  | `Lepcha -> "Lepcha"
  | `Letterlike_Symbols -> "Letterlike_Symbols"
  | `Limbu -> "Limbu"
  | `Linear_A -> "Linear_A"
  | `Linear_B_Ideograms -> "Linear_B_Ideograms"
  | `Linear_B_Syllabary -> "Linear_B_Syllabary"
  | `Lisu -> "Lisu"
  | `Low_Surrogates -> "Low_Surrogates"
  | `Lycian -> "Lycian"
  | `Lydian -> "Lydian"
  | `Mahajani -> "Mahajani"
  | `Mahjong -> "Mahjong"
  | `Malayalam -> "Malayalam"
  | `Mandaic -> "Mandaic"
  | `Manichaean -> "Manichaean"
  | `Math_Alphanum -> "Math_Alphanum"
  | `Math_Operators -> "Math_Operators"
  | `Meetei_Mayek -> "Meetei_Mayek"
  | `Meetei_Mayek_Ext -> "Meetei_Mayek_Ext"
  | `Mende_Kikakui -> "Mende_Kikakui"
  | `Meroitic_Cursive -> "Meroitic_Cursive"
  | `Meroitic_Hieroglyphs -> "Meroitic_Hieroglyphs"
  | `Miao -> "Miao"
  | `Misc_Arrows -> "Misc_Arrows"
  | `Misc_Math_Symbols_A -> "Misc_Math_Symbols_A"
  | `Misc_Math_Symbols_B -> "Misc_Math_Symbols_B"
  | `Misc_Pictographs -> "Misc_Pictographs"
  | `Misc_Symbols -> "Misc_Symbols"
  | `Misc_Technical -> "Misc_Technical"
  | `Modi -> "Modi"
  | `Modifier_Letters -> "Modifier_Letters"
  | `Modifier_Tone_Letters -> "Modifier_Tone_Letters"
  | `Mongolian -> "Mongolian"
  | `Mro -> "Mro"
  | `Music -> "Music"
  | `Multani -> "Multani"
  | `Myanmar -> "Myanmar"
  | `Myanmar_Ext_A -> "Myanmar_Ext_A"
  | `Myanmar_Ext_B -> "Myanmar_Ext_B"
  | `NB -> "NB"
  | `NKo -> "NKo"
  | `Nabataean -> "Nabataean"
  | `New_Tai_Lue -> "New_Tai_Lue"
  | `Number_Forms -> "Number_Forms"
  | `OCR -> "OCR"
  | `Ogham -> "Ogham"
  | `Ol_Chiki -> "Ol_Chiki"
  | `Old_Hungarian -> "Old_Hungarian"
  | `Old_Italic -> "Old_Italic"
  | `Old_North_Arabian -> "Old_North_Arabian"
  | `Old_Permic -> "Old_Permic"
  | `Old_Persian -> "Old_Persian"
  | `Old_South_Arabian -> "Old_South_Arabian"
  | `Old_Turkic -> "Old_Turkic"
  | `Oriya -> "Oriya"
  | `Ornamental_Dingbats -> "Ornamental_Dingbats"
  | `Osmanya -> "Osmanya"
  | `PUA -> "PUA"
  | `Pahawh_Hmong -> "Pahawh_Hmong"
  | `Palmyrene -> "Palmyrene"
  | `Pau_Cin_Hau -> "Pau_Cin_Hau"
  | `Phags_Pa -> "Phags_Pa"
  | `Phaistos -> "Phaistos"
  | `Phoenician -> "Phoenician"
  | `Phonetic_Ext -> "Phonetic_Ext"
  | `Phonetic_Ext_Sup -> "Phonetic_Ext_Sup"
  | `Playing_Cards -> "Playing_Cards"
  | `Psalter_Pahlavi -> "Psalter_Pahlavi"
  | `Punctuation -> "Punctuation"
  | `Rejang -> "Rejang"
  | `Rumi -> "Rumi"
  | `Runic -> "Runic"
  | `Samaritan -> "Samaritan"
  | `Saurashtra -> "Saurashtra"
  | `Sharada -> "Sharada"
  | `Shavian -> "Shavian"
  | `Shorthand_Format_Controls -> "Shorthand_Format_Controls"
  | `Siddham -> "Siddham"
  | `Sinhala -> "Sinhala"
  | `Sinhala_Archaic_Numbers -> "Sinhala_Archaic_Numbers"
  | `Small_Forms -> "Small_Forms"
  | `Sora_Sompeng -> "Sora_Sompeng"
  | `Specials -> "Specials"
  | `Sundanese -> "Sundanese"
  | `Sundanese_Sup -> "Sundanese_Sup"
  | `Sup_Arrows_A -> "Sup_Arrows_A"
  | `Sup_Arrows_B -> "Sup_Arrows_B"
  | `Sup_Arrows_C -> "Sup_Arrows_C"
  | `Sup_Math_Operators -> "Sup_Math_Operators"
  | `Sup_PUA_A -> "Sup_PUA_A"
  | `Sup_PUA_B -> "Sup_PUA_B"
  | `Sup_Punctuation -> "Sup_Punctuation"
  | `Sup_Symbols_And_Pictographs -> "Sup_Symbols_And_Pictographs"
  | `Super_And_Sub -> "Super_And_Sub"
  | `Sutton_SignWriting -> "Sutton_SignWriting"
  | `Syloti_Nagri -> "Syloti_Nagri"
  | `Syriac -> "Syriac"
  | `Tagalog -> "Tagalog"
  | `Tagbanwa -> "Tagbanwa"
  | `Tags -> "Tags"
  | `Tai_Le -> "Tai_Le"
  | `Tai_Tham -> "Tai_Tham"
  | `Tai_Viet -> "Tai_Viet"
  | `Tai_Xuan_Jing -> "Tai_Xuan_Jing"
  | `Takri -> "Takri"
  | `Tamil -> "Tamil"
  | `Telugu -> "Telugu"
  | `Thaana -> "Thaana"
  | `Thai -> "Thai"
  | `Tibetan -> "Tibetan"
  | `Tifinagh -> "Tifinagh"
  | `Tirhuta -> "Tirhuta"
  | `Transport_And_Map -> "Transport_And_Map"
  | `UCAS -> "UCAS"
  | `UCAS_Ext -> "UCAS_Ext"
  | `Ugaritic -> "Ugaritic"
  | `VS -> "VS"
  | `VS_Sup -> "VS_Sup"
  | `Vai -> "Vai"
  | `Vedic_Ext -> "Vedic_Ext"
  | `Vertical_Forms -> "Vertical_Forms"
  | `Warang_Citi -> "Warang_Citi"
  | `Yi_Radicals -> "Yi_Radicals"
  | `Yi_Syllables -> "Yi_Syllables"
  | `Yijing -> "Yijing"
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
