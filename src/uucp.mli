(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Unicode character properties.

    [Uucp] provides efficient access to a selection of character
    {{!props}properties} of the Unicode character database.

    Consult a {{!uminimal}minimal Unicode introduction} and
    {{!tips}tips} for Unicode processing in OCaml. Individual modules
    have sample code related to the properties.

    {e %%VERSION%% — Unicode version %%UNICODE_VERSION%% —
       {{:%%PKG_HOMEPAGE%% }homepage}}

    {3 References}
    {ul
    {- {{:http://www.unicode.org/faq/}The Unicode FAQ.}}
    {- The Unicode Consortium.
    {e {{:http://www.unicode.org/versions/latest}The Unicode Standard}}.
    (latest version)}
    {- Mark Davis, Ken Whistler.
    {e {{:http://www.unicode.org/reports/tr44/}UAX #44 Unicode Character
    Database}}. (latest version)}} *)

(** {1:props Properties}

    Consult information about the {{!distrib_omit}property distribution
    in modules and omissions}. *)

val unicode_version : string
(** [unicode_version] is the Unicode version supported by the library. *)

(** Age property. *)
module Age : sig

  (** {1:ageprop Age property} *)

  type t = [ `Unassigned | `Version of int * int ]
  (** The type for character age. *)

  val compare : t -> t -> int
  (** [compare a a'] is [Pervasives.compare a a'] *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf a] prints an unspecified representation of [a] on [ppf]. *)

  val age : Uchar.t -> t
  (** [age u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Age}Age} property. *)
end

(** Alphabetic property. *)
module Alpha : sig

  (** {1:alphaprop Alphabetic property} *)

  val is_alphabetic : Uchar.t -> bool
  (** [is_alphabetic u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Alphabetic}Alphabetic}
      property. *)
end

(** Block property and block ranges.

    {3 References}
    {ul
    {- {{:http://www.unicode.org/faq/blocks_ranges.html}The Unicode
    blocks and ranges FAQ}.}} *)
module Block : sig

(** {1:blockprop Blocks} *)

  type t = [
    | `ASCII
    | `Adlam
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
    | `Bhaiksuki
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
    | `CJK_Ext_F
    | `CJK_Radicals_Sup
    | `CJK_Strokes
    | `CJK_Symbols
    | `Carian
    | `Caucasian_Albanian
    | `Chakma
    | `Cham
    | `Cherokee
    | `Cherokee_Sup
    | `Chess_Symbols
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
    | `Cyrillic_Ext_C
    | `Cyrillic_Sup
    | `Deseret
    | `Devanagari
    | `Devanagari_Ext
    | `Diacriticals
    | `Diacriticals_Ext
    | `Diacriticals_For_Symbols
    | `Diacriticals_Sup
    | `Dingbats
    | `Dogra
    | `Domino
    | `Duployan
    | `Early_Dynastic_Cuneiform
    | `Egyptian_Hieroglyph_Format_Controls
    | `Egyptian_Hieroglyphs
    | `Elbasan
    | `Elymaic
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
    | `Georgian_Ext
    | `Georgian_Sup
    | `Glagolitic
    | `Glagolitic_Sup
    | `Gothic
    | `Grantha
    | `Greek
    | `Greek_Ext
    | `Gujarati
    | `Gunjala_Gondi
    | `Gurmukhi
    | `Half_And_Full_Forms
    | `Half_Marks
    | `Hangul
    | `Hanifi_Rohingya
    | `Hanunoo
    | `Hatran
    | `Hebrew
    | `Hiragana
    | `IDC
    | `IPA_Ext
    | `Ideographic_Symbols
    | `Imperial_Aramaic
    | `Indic_Number_Forms
    | `Indic_Siyaq_Numbers
    | `Inscriptional_Pahlavi
    | `Inscriptional_Parthian
    | `Jamo
    | `Jamo_Ext_A
    | `Jamo_Ext_B
    | `Javanese
    | `Kaithi
    | `Kana_Ext_A
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
    | `Makasar
    | `Malayalam
    | `Mandaic
    | `Manichaean
    | `Marchen
    | `Masaram_Gondi
    | `Math_Alphanum
    | `Math_Operators
    | `Mayan_Numerals
    | `Medefaidrin
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
    | `Mongolian_Sup
    | `Mro
    | `Multani
    | `Music
    | `Myanmar
    | `Myanmar_Ext_A
    | `Myanmar_Ext_B
    | `NB (** Non_block *)
    | `NKo
    | `Nabataean
    | `Nandinagari
    | `New_Tai_Lue
    | `Newa
    | `Number_Forms
    | `Nushu
    | `Nyiakeng_Puachue_Hmong
    | `OCR
    | `Ogham
    | `Ol_Chiki
    | `Old_Hungarian
    | `Old_Italic
    | `Old_North_Arabian
    | `Old_Permic
    | `Old_Persian
    | `Old_Sogdian
    | `Old_South_Arabian
    | `Old_Turkic
    | `Oriya
    | `Ornamental_Dingbats
    | `Osage
    | `Osmanya
    | `Ottoman_Siyaq_Numbers
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
    | `Small_Kana_Ext
    | `Sogdian
    | `Sora_Sompeng
    | `Soyombo
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
    | `Symbols_And_Pictographs_Ext_A
    | `Syriac
    | `Syriac_Sup
    | `Tagalog
    | `Tagbanwa
    | `Tags
    | `Tai_Le
    | `Tai_Tham
    | `Tai_Viet
    | `Tai_Xuan_Jing
    | `Takri
    | `Tamil
    | `Tamil_Sup
    | `Tangut
    | `Tangut_Components
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
    | `Wancho
    | `Warang_Citi
    | `Yi_Radicals
    | `Yi_Syllables
    | `Yijing
    | `Zanabazar_Square
  ]
  (** The type for blocks. The value [`NB] is for characters that are not
      yet assigned to a block. *)

  val compare : t -> t -> int
  (** [compare b b'] is [Pervasives.compare b b']. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf b] prints an unspecified representation of [b] on [ppf]. *)

  val blocks : (t * (Uchar.t * Uchar.t)) list
  (** [blocks] is the list of blocks sorted by increasing range order.
      Each block appears exactly once in the list except
      [`NB] which is not part of this list as it is not a block. *)

  val block : Uchar.t -> t
  (** [block u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Block}Block} property. *)
end

(** Break properties.

    These properties are mainly for the Unicode text segmentation and line
    breaking algorithm.

    {3 References}
    {ul
    {- Mark Davis.
    {e {{:http://www.unicode.org/reports/tr29/}UAX #29 Unicode Text
    Segmentation}}. (latest version)}
    {- Andy Heninger.
    {e {{:http://www.unicode.org/reports/tr14/}UAX #14 Unicode Line Breaking
    Algorithm}}. (latest version)}
    {- Ken Lunde 小林劍.
    {e {{:http://www.unicode.org/reports/tr11/}UAX #11 East Asian width.}
    (latest version)}}} *)
module Break : sig

  (** {1:line_break Line break} *)

  type line =
    [ `AI | `AL | `B2 | `BA | `BB | `BK | `CB | `CJ | `CL | `CM | `CP
    | `CR | `EX | `EB | `EM | `GL | `H2 | `H3 | `HL | `HY | `ID | `IN
    | `IS | `JL | `JT | `JV | `LF | `NL | `NS | `NU | `OP | `PO | `PR
    | `QU | `RI | `SA | `SG | `SP | `SY | `WJ | `XX | `ZW | `ZWJ ]
  (** The type for line breaks. *)

  val pp_line : Format.formatter -> line -> unit
  (** [pp_line ppf l] prints an unspecified representation of [l] on
      [ppf]. *)

  val line : Uchar.t -> line
  (** [line u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Line_Break}line break}
      property. *)

  (** {1:grapheme_cluster_break Grapheme cluster break} *)

  type grapheme_cluster =
    [ `CN | `CR | `EX | `EB | `EBG | `EM | `GAZ | `L | `LF | `LV | `LVT | `PP
    | `RI | `SM | `T | `V | `XX | `ZWJ ]
  (** The type for grapheme cluster breaks. *)

  val pp_grapheme_cluster : Format.formatter -> grapheme_cluster -> unit
  (** [pp_grapheme_cluster ppf g] prints an unspecified representation of [g]
      on [ppf]. *)

  val grapheme_cluster : Uchar.t -> grapheme_cluster
  (** [grapheme_cluster u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Grapheme_Cluster_Break}grapheme
      cluster break} property. *)

  (** {1:word_break Word break} *)

  type word =
    [ `CR | `DQ | `EX | `EB | `EBG | `EM | `Extend | `FO | `GAZ | `HL | `KA
    | `LE | `LF | `MB | `ML | `MN | `NL | `NU | `RI | `SQ | `WSegSpace
    | `XX | `ZWJ ]
  (** The type for word breaks. *)

  val pp_word : Format.formatter -> word -> unit
  (** [pp_grapheme_cluster ppf g] prints an unspecified representation of [g]
      on [ppf]. *)

  val word : Uchar.t -> word
  (** [world u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Word_Break}word break}
      property. *)

  (** {1:sentence_break Sentence break} *)

  type sentence =
    [ `AT | `CL | `CR | `EX | `FO | `LE | `LF | `LO | `NU | `SC | `SE | `SP
    | `ST | `UP | `XX ]
  (** The type for sentence breaks. *)

  val pp_sentence : Format.formatter -> sentence -> unit
  (** [pp_grapheme_cluster ppf g] prints an unspecified representation of [g]
      on [ppf]. *)

  val sentence : Uchar.t -> sentence
  (** [sentence u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Sentence_Break}sentence break}
      property. *)

  (** {1:east_asian_width East Asian width} *)

  type east_asian_width = [ `A | `F | `H | `N | `Na | `W ]
  (** The type for East Asian widths. *)

  val pp_east_asian_width : Format.formatter -> east_asian_width -> unit
  (** [pp_east_asian_width ppf w] prints an unspecified representation of
      [w] on [ppf]. *)

  val east_asian_width : Uchar.t -> east_asian_width
  (** [east_asian_width u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#East_Asian_Width}East Asian
      width} property. *)

  (** {1:terminal_width Terminal width} *)

  val tty_width_hint: Uchar.t -> int
  (** [tty_width_hint u] approximates [u]'s column width as rendered by a
      typical character terminal.

      The current implementation of the function returns either [0],
      [1], [2] or [-1]. The value [-1] is only returned for scalar
      values for which the property is non-sensical; clients are
      expected to sanitize their inputs and not to use the function
      with these scalar values which are those in range U+0001-U+001F
      ({b C0} controls without U+0000) and U+007F-U+009F (DELETE and
      {b C1} controls).

      {b Note.} Converting a string to
      {{:http://unicode.org/glossary/#normalization_form_c}normalization
      form C} before folding this function over its scalar values
      will, in general, yield better approximations (e.g. on Hangul).

      {b Warning.} This is not a normative property and only a
      heuristic. If you find yourself using this function please read
      carefully the following lines.

      This function is the moral equivalent of POSIX
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/wcwidth.html}
      [wcwidth]}, in that its purpose is to help align text displayed by a
      character terminal. It mimics [wcwidth], as widely implemented, in yet
      another way: it is {e mostly wrong}.

      Computing column width is a surprisingly difficult task in general. Much
      of the software infrastructure still carries legacy assumptions about the
      nature of text harking back to the ASCII era. Different terminal emulators
      attempt to cope with general Unicode text in different ways, creating a
      fundamental problem: width of text fragments will vary across terminal
      emulators, with no way of getting feedback from the output layer back
      into the text-producing layer.

      For example: on a modern Linux system, a collection of terminals
      will disagree on some or all of U+00AD, U+0CBF, and
      U+2029. They will likewise disagree about unassigned
      characters (category {e Cn}), sometimes contradicting the
      system's [wcwidth] (e.g. U+0378, U+0530).  Terminals using
      bare {{:http://cgit.freedesktop.org/xorg/lib/libXft}libxft}
      will display complex scripts differently from terminals using
      {{:http://www.freedesktop.org/wiki/Software/HarfBuzz}HarfBuzz},
      and the rendering on OS X will be slightly different from both.

      [tty_width_hint] uses a simple and predictable width algorithm, based
      on Markus Kuhn's {{:https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c}
      portable [wcwidth]}:
      {ul
      {- Scalar values in the ranges U+0001-U+001F ({b C0} controls without
         U+0000) and U+007F-U+009F (DELETE and {b C1} controls) have undefined
         width ([-1]).}
      {- Characters with {{:http://www.unicode.org/reports/tr11/tr11-29.html}
         East Asian Width} {e Fullwidth} or {e Wide} have a width of [2].}
      {- Characters with
         {{:http://unicode.org/glossary/#general_category}General Category}
         {e Mn}, {e Me}, {e Cf} and U+0000 have a width of [0].}
      {- {e Most} other characters have a width of [1], including {e Cn}.}}

      This approach works well, in that it gives results generally consistent
      with a wide range of terminals, for
      {{:https://en.wikipedia.org/wiki/Alphabet}alphabetic} scripts, and for
      east Asian {{:https://en.wikipedia.org/wiki/Syllabary}syllabic} and
      {{:https://en.wikipedia.org/wiki/Logogram}logographic} scripts in
      non-decomposed form. Support varies for
      {{:https://en.wikipedia.org/wiki/Abjad}abjad} scripts in the presence of
      vowel marks, and it mostly breaks down on
      {{:https://en.wikipedia.org/wiki/Abugida}abugidas}.

      Moreover, non-text symbols like
      {{:http://unicode.org/emoji/charts/full-emoji-list.html}Emoji}
      or {{:http://unicode.org/charts/PDF/U4DC0.pdf}Yijing hexagrams}
      will be incorrectly classified as [1]-wide, but this in fact
      agrees with their rendering on many terminals.

      Clients should not over-rely on [tty_width_hint]. It provides a
      best-effort approximation which will sometimes fail in
      practice. *)


  (** {1:break_low Low level interface} *)

  (** Low level interface.

      This interface may be useful for table based implementers of
      segmenters. For each kind of break, property values are
      assigned integer values starting from [0]. An array
      allows to recover the high-level representation of the
      corresponding property value. *)
module Low : sig

    (** {1 Low level access to break properties}

        {b Warning.} Do not mutate these array. *)

    val line : Uchar.t -> int
    (** [line u] is an integer that can be used with {!line_of_int}. *)

    val line_max : int
    (** [line_max] is the maximal value returned by {!line}. *)

    val line_of_int : line array
    (** [line_of_int.(i)] is the line break property value corresponding
        to [i]. *)

    val grapheme_cluster : Uchar.t -> int
    (** [grapheme_cluster u] is an integer that can be used with
        {!grapheme_cluster_of_int}. *)

    val grapheme_cluster_max : int
    (** [grapheme_cluster_max] is the maximal value returned by
        {!grapheme_cluster}. *)

    val grapheme_cluster_of_int : grapheme_cluster array
    (** [grapheme_cluster_of_int.(i)] is the grapheme cluster break property
        value corresponding to [i]. *)

    val word : Uchar.t -> int
    (** [word u] is an integer that can be used with {!word_of_int}. *)

    val word_max : int
    (** [word_max] is the maximal value returned by {!word}. *)

    val word_of_int : word array
    (** [word_of_int.(i)] is the word break property value
        corresponding to [i]. *)

    val sentence : Uchar.t -> int
    (** [sentence u] is an integer that can be used with {!sentence_of_int}. *)

    val sentence_max : int
    (** [sentence_max] is the maximal value returned by {!sentence}. *)

    val sentence_of_int : sentence array
    (** [sentence_of_int.(i)] is the sentence break property value
        corresponding to [i]. *)
  end
end

(** Case properties, mappings and foldings.

    These properties can implement Unicode's default case detection,
    case conversion and caseless equality over Unicode text, see the
    {{!caseexamples}examples}.

    {3 References}
    {ul
    {- {{:http://unicode.org/faq/casemap_charprop.html#casemap}
        The Unicode case mapping FAQ.}}
    {- {{:http://www.unicode.org/charts/case/}The Unicode case mapping
       charts.}}} *)
module Case : sig

  (** {1:caseprops Case properties} *)

  val is_lower : Uchar.t -> bool
  (** [is_lower u] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Lowercase}Lowercase} derived
      property. *)

  val is_upper : Uchar.t -> bool
  (** [is_upper u] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Uppercase}Uppercase} derived
      property. *)

  val is_cased : Uchar.t -> bool
  (** [is_cased u] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Cased}Cased} derived property. *)

  val is_case_ignorable : Uchar.t -> bool
  (** [is_case_ignorable] is [true] iff [u] has the
      {{:http://www.unicode.org/reports/tr44/#Case_Ignorable}Case_Ignorable}
      derived property. *)

  (** {1:casemapfold Case mappings and foldings}

      These character mapping functions return [`Self]
      whenever a character maps to itself. *)

  (** Case mappings.  *)
  module Map : sig

    (** {1:casemaps Case mappings} *)

    val to_lower : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [to_lower u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Lowercase_Mapping}
        Lowercase_Mapping} property. *)

    val to_upper : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [to_upper u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Uppercase_Mapping}
        Uppercase_Mapping} property. *)

    val to_title : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [to_title u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Titlecase_Mapping}
        Titlecase_Mapping} property. *)
  end

  (** Case folding. *)
  module Fold : sig

    (** {1:casefolding Case folding} *)

    val fold : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [fold u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#Case_Folding}Case_Folding}
        property. *)
  end

  (** NFKC case folding. *)
  module Nfkc_fold : sig

    (** {1:nfkcfold NFKC Case folding} *)

    val fold : Uchar.t -> [ `Self | `Uchars of Uchar.t list ]
    (** [fold u] is [u]'s
        {{:http://www.unicode.org/reports/tr44/#NFKC_Casefold}NFKC_Casefold}
        property. *)
  end

  (** {1:caseexamples Examples}

      These examples use {!Uutf} to fold over the characters of UTF-8
      encoded OCaml strings and to UTF-8 encode mapped characters in
      an OCaml {!Buffer.t} value.

      {2:caseconversion Default case conversion on UTF-8 strings}

      The value [casemap_utf_8 cmap s] is the UTF-8 encoded string
      resulting from applying the character map [cmap] to every character
      of the UTF-8 encoded string [s].
{[
let cmap_utf_8 cmap s =
  let b = Buffer.create (String.length s * 2) in
  let rec add_map _ _ u =
    let u = match u with `Malformed _ -> Uutf.u_rep | `Uchar u -> u in
    match cmap u with
    | `Self -> Uutf.Buffer.add_utf_8 b u
    | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us
  in
  Uutf.String.fold_utf_8 add_map () s; Buffer.contents b
]}
      Using the function [cmap_utf_8], Unicode's default case
      conversions can be implemented with:
{[
let lowercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_lower s
let uppercase_utf_8 s = cmap_utf_8 Uucp.Case.Map.to_upper s
]}
      However strictly speaking [lowercase_utf_8] is not conformant
      as it doesn't handle the context sensitive mapping of capital
      sigma U+03A3 to final sigma U+03C2.

      Note that applying Unicode's default case algorithms to a normalized
      string does not preserve its normalization form.

      {2:caselesseq Default caseless matching (equality) on UTF-8 strings}

      These examples use {!Uunf} to normalize character sequences

      Unicode canonical caseless matching (D145) is defined by
      normalizing to NFD, applying the Case_Folding mapping, normalizing
      again to NFD and test the result for binary equality:

{[
let canonical_caseless_key s =
  let b = Buffer.create (String.length s * 2) in
  let to_nfd_and_utf_8 =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add `Await
    in
    add
  in
  let add =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
    | `Await | `End -> ()
    | `Uchar u ->
        begin match Uucp.Case.Fold.fold u with
        | `Self -> to_nfd_and_utf_8 (`Uchar u)
        | `Uchars us -> List.iter (fun u -> to_nfd_and_utf_8 (`Uchar u)) us
        end;
        add `Await
    in
    add
  in
  let add_uchar _ _ = function
  | `Malformed  _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  to_nfd_and_utf_8 `End;
  Buffer.contents b

let canonical_caseless_eq s0 s1 =
  canonical_caseless_key s0 = canonical_caseless_key s1
]}
      Unicode's caseless matching for identifiers (D147, see also
      {{:http://www.unicode.org/reports/tr31/}UAX 31}) is defined
      by normalizing to NFD, applying the NFKC_Casefold mapping and test
      the result for binary equality:
{[
let id_caseless_key s =
  let b = Buffer.create (String.length s * 3) in
  let n = Uunf.create `NFD in
  let rec add v = match Uunf.add n v with
  | `Await | `End -> ()
  | `Uchar u ->
      begin match Uucp.Case.Nfkc_fold.fold u with
      | `Self -> Uutf.Buffer.add_utf_8 b u; add `Await
      | `Uchars us -> List.iter (Uutf.Buffer.add_utf_8 b) us; add `Await
      end
  in
  let add_uchar _ _ = function
  | `Malformed  _ -> add (`Uchar Uutf.u_rep)
  | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  Buffer.contents b

let id_caseless_eq s0 s1 = id_caseless_key s0 = id_caseless_key s1
]}
*)
end

(** CJK properties.

    {3 References}
    {ul
    {- {{:http://www.unicode.org/faq/han_cjk.html}
    The Unicode Chinese and Japanese FAQ.}}
    {- {{:http://www.unicode.org/faq/korean.html}
    The Unicode Korean FAQ.}}} *)
module Cjk : sig

  (**  {1:cjkprops CJK properties} *)

  val is_ideographic : Uchar.t -> bool
  (** [is_ideographic u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Ideographic}Ideographic}
      property. *)

  val is_ids_bin_op : Uchar.t -> bool
  (** [is_ids_bin_op u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#IDS_Binary_Operator}
      IDS_Binary_Operator} property. *)

  val is_ids_tri_op : Uchar.t -> bool
  (** [is_ids_tri_op u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#IDS_Trinary_Operator}
      IDS_Trinary_Operator} property. *)

  val is_radical : Uchar.t -> bool
  (** [is_radical u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Radical}Radical}
      property. *)

  val is_unified_ideograph : Uchar.t -> bool
  (** [is_unified_ideograph u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Unified_Ideograph}
      Unified_Ideograph} property. *)
end

(** Function and graphics properties. *)
module Func : sig

  (** {1:funcprops Function and graphics properties} *)

  val is_dash : Uchar.t -> bool
  (** [is_dash u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Dash}Dash}
      property. *)

  val is_diacritic : Uchar.t -> bool
  (** [is_diacritic u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Diacritic}Diacritic}
      property. *)

  val is_extender : Uchar.t -> bool
  (** [is_extender u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Extender}Extender}
      property. *)

  val is_grapheme_base : Uchar.t -> bool
  (** [is_grapheme_base u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Grapheme_Base}Grapheme_Base}
      property. *)

  val is_grapheme_extend : Uchar.t -> bool
  (** [is_grapheme_extend u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Grapheme_Extend}Grapheme_Extend}
      property. *)

  val is_math : Uchar.t -> bool
  (** [is_math u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Math}Math}
      property. *)

  val is_quotation_mark : Uchar.t -> bool
  (** [is_quotation_mark u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Quotation_Mark}Quotation_Mark}
      property. *)

  val is_soft_dotted : Uchar.t -> bool
  (** [is_soft_dotted u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Soft_Dotted}Soft_Dotted}
      property. *)

  val is_terminal_punctuation : Uchar.t -> bool
  (** [is_terminal_punctuation u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Terminal_Punctuation}
      Terminal_Punctuation} property. *)

  val is_regional_indicator : Uchar.t -> bool
  (** [is_regional_indicator u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Regional_Indicator}
      Regional_indicator} property. *)

  val is_join_control : Uchar.t -> bool
  (** [is_join_control u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Join_Control}Join_Control}
      property. *)
end

(** General category property. *)
module Gc : sig

  (** {1:gcprop General category property} *)

  type t =
    [ `Cc | `Cf | `Cn | `Co | `Cs | `Ll | `Lm | `Lo | `Lt | `Lu | `Mc
    | `Me | `Mn | `Nd | `Nl | `No | `Pc | `Pd | `Pe | `Pf | `Pi | `Po
    | `Ps | `Sc | `Sk | `Sm | `So | `Zl | `Zp | `Zs ]
  (** The type for general categories. *)

  val compare : t -> t -> int
  (** [compare c c'] is [Pervasives.compare s s']. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf c] prints an unspecified representation of [c] on [ppf]. *)

  val general_category : Uchar.t -> t
  (** [general_category u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#General_Category}
      General_Category} property. *)
end

(** General properties. *)
module Gen : sig

  (** {1:genprops General properties} *)

  val is_default_ignorable : Uchar.t -> bool
  (** [is_default_ignorable u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Default_Ignorable_Code_Point}
       Default_Ignorable_Code_Point} property. *)

  val is_deprecated : Uchar.t -> bool
  (** [is_deprecated u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Deprecated}
       Deprecated} property. *)

  val is_logical_order_exception : Uchar.t -> bool
  (** [is_logical_order_exception u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Logical_Order_Exception}
      Logical_Order_Exception} property. *)

  val is_non_character : Uchar.t -> bool
  (** [is_non_character u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Noncharacter_Code_Point}
      Noncharacter_Code_Point} property. *)

  val is_variation_selector : Uchar.t -> bool
  (** [is_variation_selector u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Variation_Selector}
      Variation_Selector} property. See the
      {{:http://www.unicode.org/faq/vs.html}Variation Sequences FAQ}. *)
end

(** Hangul properties. *)
module Hangul : sig

  (** {1:hangul_syllable_type Hangul syllable type property} *)

  type syllable_type = [ `L | `V | `T | `LV | `LVT | `NA ]
  (** The type for hangul syllable types. *)

  val pp_syllable_type : Format.formatter -> syllable_type -> unit
  (** [pp_syllable_type ppf s] prints an unspecified representation of
      [s] on [ppf]. *)

  val syllable_type : Uchar.t -> syllable_type
  (** [syllable_type u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Hangul_Syllable_Type}
      Hangul_Syllable_type} property. *)
end

(** Identifier properties.

    {3 References}
    {ul
    {- Mark Davis.
    {e {{:http://www.unicode.org/reports/tr31/}UAX #31
       Unicode Identifier and Pattern Syntax}}. (latest version)}} *)
module Id : sig

  (** {1:idprops Identifier properties} *)

  val is_id_start : Uchar.t -> bool
  (** [is_id_start u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#ID_Start}ID_Start}
      property. *)

  val is_id_continue : Uchar.t -> bool
  (** [is_id_continue u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#ID_Continue}ID_Continue}
      property. *)

  val is_xid_start : Uchar.t -> bool
  (** [is_xid_start u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#XID_Start}XID_Start}
      property. *)

  val is_xid_continue : Uchar.t -> bool
  (** [is_xid_continue u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#XID_Continue}XID_Continue}
      property. *)

  (** {1:patprops Pattern syntax properties} *)

  val is_pattern_syntax : Uchar.t -> bool
  (** [is_pattern_syntax u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Pattern_Syntax}Pattern_Syntax}
      property. *)


  val is_pattern_white_space : Uchar.t -> bool
  (** [is_pattern_white_space u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Pattern_White_Space}
      Pattern_White_Space} property. *)
end

(** Name and name alias properties.

    {3 References}
    {ul
    {- {{:http://unicode.org/faq/casemap_charprop.html#nameprop}
    The Unicode names FAQ}.}
    {- The Unicode consortium.
       {{:https://unicode.org/charts/nameslist/index.html}
       The Unicode names charts}}} *)
module Name : sig

  (** {1:nameprop Names} *)

  val name : Uchar.t -> string
  (** [name u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Name}Name} property. *)

  (** {1:namealiasprop Name aliases} *)

  type alias_tag =
    [ `Abbreviation | `Alternate | `Control | `Correction | `Figment ]

  val pp_alias_tag : Format.formatter -> alias_tag -> unit
  (** [pp_alias_tag t] prints an unspecified representation of [t]
      on [ppf]. *)

  val name_alias : Uchar.t -> (alias_tag * string) list
  (** [name_alias u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Name_Alias}Name_Alias}
      property. *)
end

(** Numeric properties. *)
module Num : sig

  (** {1:hexprop Hex digits} *)

  val is_ascii_hex_digit : Uchar.t -> bool
  (** [is_ascii_hex_digit u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#ASCII_Hex_Digit}ASCII_Hex_Digit}
      property. *)

  val is_hex_digit : Uchar.t -> bool
  (** [is_ascii_hex_digit u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#Hex_Digit}Hex_Digit}
      property. *)

  (** {1:numtypeprop Numeric type} *)

  type numeric_type = [ `De | `Di | `None | `Nu ]
  (** The type for numeric types. *)

  val pp_numeric_type : Format.formatter -> numeric_type -> unit
  (** [pp_numeric_type ppf n] prints an unspecified representation of
      [n] on [ppf]. *)

  val numeric_type : Uchar.t -> numeric_type
  (** [numeric_type u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Numeric_Type}
      Numeric_Type} property. *)

  (** {1:numvalueprop Numeric value} *)

  type numeric_value = [ `Frac of int * int | `NaN | `Num of int64 ]
  (** The type for numeric values. *)

  val pp_numeric_value : Format.formatter -> numeric_value -> unit
  (** [pp_numeric_value ppf n] prints an unspecified representation of
      [n] on [ppf]. *)

  val numeric_value : Uchar.t -> [ `Frac of int * int | `NaN | `Num of int64 ]
  (** [numeric_type u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Numeric_Value}
      Numeric_Value} property. *)
end

(** Script and script extensions properties.

    {3 References}
    {ul
    {- Mark Davis, Ken Whistler.
    {{:http://www.unicode.org/reports/tr24/}{e Unicode script property}}.
    (latest version)}
    {- {{:http://www.unicode.org/charts/script/index.html}The Unicode script
    charts}.}} *)
module Script : sig

  (** {1:scriptprop Script} *)

  type t = [
  | `Adlm
  | `Aghb
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
  | `Bhks
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
  | `Dogr
  | `Dsrt
  | `Dupl
  | `Egyp
  | `Elba
  | `Elym
  | `Ethi
  | `Geor
  | `Glag
  | `Gong
  | `Gonm
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
  | `Hmng
  | `Hmnp
  | `Hrkt
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
  | `Maka
  | `Mand
  | `Mani
  | `Marc
  | `Medf
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
  | `Nand
  | `Narb
  | `Nbat
  | `Newa
  | `Nkoo
  | `Nshu
  | `Ogam
  | `Olck
  | `Orkh
  | `Orya
  | `Osge
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
  | `Rohg
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
  | `Sogd
  | `Sogo
  | `Sora
  | `Soyo
  | `Sund
  | `Sylo
  | `Syrc
  | `Tagb
  | `Takr
  | `Tale
  | `Talu
  | `Taml
  | `Tang
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
  | `Wcho
  | `Xpeo
  | `Xsux
  | `Yiii
  | `Zanb
  | `Zinh
  | `Zyyy
  | `Zzzz
  ]
  (** The type for scripts. *)

  val compare : t -> t -> int
  (** [compare s s'] is [Pervasives.compare s s']. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf s] prints an unspecified representation of [s] on [ppf]. *)

  val script : Uchar.t -> t
  (** [script u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Script}Script} property. *)

  val script_extensions : Uchar.t -> t list
  (** [script_extension u] is [u]'s
      {{:http://www.unicode.org/reports/tr44/#Script_Extensions}
      Script_Extensions} property. The list is never empty. *)
end

(** White space property. *)
module White : sig

  (**  {1:whiteprop White space property} *)

  val is_white_space : Uchar.t -> bool
  (** [is_white_space u] is [true] if [u] has the
      {{:http://www.unicode.org/reports/tr44/#White_Space}White_Space}
      property. *)
end

(** {1:distrib_omit Property module distribution and omissions}

    Properties are approximatively distributed in modules by scope of use
    like in this
    {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}property
    index table}. However some subset of properties
    live in their own modules.

    Obsolete and
    {{:http://www.unicode.org/reports/tr44/#Deprecated_Property_Table}
    deprecated} properties are
    omitted.  So are those related to normalization, shaping and
    bidirectionality. Here is the full list of omitted properties,
    if you think one of these property should be added get in touch
    with a rationale.
    {ul
    {- Case.
       {{:http://www.unicode.org/reports/tr44/#Simple_Lowercase_Mapping}
       Simple_Lowercase_Mapping},
       {{:http://www.unicode.org/reports/tr44/#Simple_Uppercase_Mapping}
       Simple_Uppercase_Mapping},
       {{:http://www.unicode.org/reports/tr44/#Simple_Titlecase_Mapping}
       Simple_Titlecase_Mapping},
       {{:http://www.unicode.org/reports/tr44/#Simple_Case_Folding}
       Simple_Case_folding},
       {{:http://www.unicode.org/reports/tr44/#CWL}
       Changes_When_Lowercased},
       {{:http://www.unicode.org/reports/tr44/#CWU}
       Changes_When_Uppercased},
       {{:http://www.unicode.org/reports/tr44/#CWT}
       Changes_When_Titlecased},
       {{:http://www.unicode.org/reports/tr44/#CWCF}
       Changes_When_Casefolded},
       {{:http://www.unicode.org/reports/tr44/#CWCM}
       Changes_When_Casemapped}.}
    {- Normalization. All properties under that section name in
       {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}
       this table}.}
    {- Shaping and rendering.
       {{:http://www.unicode.org/reports/tr44/#Joining_Group}Joining_Group},
       {{:http://www.unicode.org/reports/tr44/#Joining_Type}Joining_Type},
       {{:http://www.unicode.org/reports/tr44/#Vertical_Orientation}Vertical_Orientation},
       {{:http://www.unicode.org/reports/tr44/#Indic_Syllabic_Category}
       Indic_Syllabic_Category},
       {{:http://www.unicode.org/reports/tr44/#Indic_Positional_Category}
       Indic_Positional_Category},
       {{:http://www.unicode.org/reports/tr44/#Prepended_Concatenation_Mark}Prepended_Concatenation_Mark}}
    {- Bidirectional. All properties under that section name in
       {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}
       this table}.}
    {- CJK.
      {{:http://www.unicode.org/reports/tr44/#Unicode_Radical_Stroke}
      Unicode_Radical_Stroke},
      {{:http://www.unicode.org/reports/tr44/#Equivalent_Unified_Ideograph}
      Equivalent_Unified_Ideograph} and all the properties of the
      {{:http://www.unicode.org/reports/tr38/}Unicode HAN Database}.}
    {- Miscellaneous.
       {{:http://www.unicode.org/reports/tr44/#STerm}STerm}.}
    {- Contributory properties. All properties under that section in
       {{:http://www.unicode.org/reports/tr44/#Property_Index_Table}this
       table.}}} *)

(** {1:uminimal Minimal Unicode introduction}

    {2:characters Characters — if they exist}

    The purpose of Unicode is to have a universal way of representing
    characters of writing systems known to the world in computer
    systems. Defining the notion of character is a very complicated
    question with both philosophical and political implications. To
    side step these issues, we only talk about characters from a
    programmer's point of view and simply say that the purpose of
    Unicode is to assign meaning to the integers of a well-defined
    integer range.

    This range is called the Unicode {e codespace}, it spans from
    [0x0000] to [0x10FFFF] and its boundaries are cast in
    stone. Members of this range are called Unicode {e code points}.
    Note that an OCaml [int] value can represent them on both 32- and
    64-bit platforms.

    There's a lot of (non-exclusive)
    {{:http://www.unicode.org/glossary/}terminology} predicates that
    can be applied to code points. I will only mention the most useful
    ones here.

    First there are the {e reserved} or {e unassigned} code points,
    those are the integers to which the standard doesn't assign any
    meaning {e yet}. They are reserved for future assignment and may
    become meaningful in newer versions of the standard. Be aware that
    once a code point has been assigned (aka as {e encoded}) by the
    standard most of its properties may never change again, see the
    {{:http://www.unicode.org/policies/stability_policy.html}stability
    policy} for details.

    A very important subset of code points are the Unicode {e scalar
    values}, these are the code points that belong to the ranges
    [0x0000]…[0xD7FF] and [0xE000]…[0x10FFFF]. This is the complete
    Unicode codespace minus the range [0xD800]…[0xDFFF] of so called
    {e surrogate} code points, a hack to be able to encode all scalar
    values in UTF-16 (more on that below).

    Scalar values are what I call, by a {b total abuse of
    terminology}, the Unicode characters; it is what a proper [uchar]
    type should represent. From a programmer's point of view they are
    the sole integers you will have to deal with during processing and
    the only code points that you are allowed to serialize and
    deserialize to valid Unicode byte sequences. Since OCaml 4.03 the
    standard library defines an {!Uchar.t} type to represent them.

    Unicode uses a standard notation to denote code points in running
    text. A code point is expressed as U+n where {e n} is four to six
    uppercase hexadecimal digits with leading zeros omitted unless the
    code point has fewer than four digits (in [printf] words
    ["U+%04X"]).  For example the code point bounds are expressed by
    U+0000 and U+10FFFF and the surrogate bounds by U+D800 and U+DFFF.

    {2:assignements Interlude — what is assigned ?}

    Lots of the world's scripts are encoded in the standard. The
    {{:http://www.unicode.org/charts/}code charts} give a precise idea
    of the coverage.

    In order to be sucessful Unicode decided to be inclusive and to
    contain pre-existing international and national standards. For
    example the scalar values from U+0000 to U+007F correspond exactly
    to the code values of characters encoded by the US-ASCII standard,
    while those from U+0000 to U+00FF correspond exactly to the code
    values of ISO-8859-1 (latin1). Many other standard are injected
    into the codespace but their map to Unicode scalar values may not be
    as straightforward as the two examples given above.

    One thing to be aware of is that because of the inclusive nature
    of the standard the same abstract character may be represented in
    more than one way by the standard. A simple example is the latin
    character "é", which can either be represented by the single
    scalar value U+00E9 or by the {e sequence} of scalar values
    <U+0065, U+0301> that is a latin small letter "e" followed by the
    combining acute accent "´". This non uniqueness of representation
    is problematic, for example whenever you want to test sequences of
    scalar values for equality. Unicode solves this by defining
    equivalence classes between sequences of scalar values, this is
    called Unicode normalization and we will talk about it later.

    Another issue is character spoofing. Many encoded characters
    ressemble each other when displayed but have different scalar
    values and meaning. The
    {{:http://www.unicode.org/faq/security.html}Unicode Security FAQ}
    has more information and pointers about these issues.

    {2:serializing Serializing integers — UTF-X}

    There is more than one way of representing a large integer as a
    sequence of bytes. The Unicode standard defines seven {e encoding
    schemes}, also known as Unicode transformation formats (UTF), that
    precisely define how to encode and decode {e scalar values} — take
    note, scalar values, {b not code points} — as byte sequences.

    {ul
    {- UTF-8, a scalar value is represented by a sequence of one
       to 4 bytes. One of the valuable property of UTF-8 is that
       it is compatible with the encoding of US-ASCII: the one byte
       sequences are solely used for encoding the 128 scalar
       value U+0000 to U+007F which correspond exactly to the US-ASCII code
       values. Any scalar value stricly greater than U+007F will use more than
       one byte.}
    {- UTF-16BE, a scalar value is either represented by one
       16 bit big-endian integer if its scalar value fits or by
       two surrogate code points encoded as 16 bit big-endian integers (how
       exactly is beyond the scope of this introduction).}
    {- UTF-16LE is like UTF-16BE but uses little-endian encoded integers.}
    {- UTF-16 is either UTF-16BE or UTF-16LE. The endianness is
       determined by looking at the two initial bytes of the data
       stream:
       {ol
       {- If they encode a byte order mark character (BOM,
       U+FEFF) they will be either [(0xFF,0xFE)], indicating
       UTF-16LE, or [(0xFE,0xFF)] indicating UTF-16BE.}
       {- Otherwise UTF-16BE is assumed.}}}
    {- UTF-32BE, a scalar value is represented by one 32 bit big-endian
       integer.}
    {- UTF-32LE is like UTF-32BE but uses little-endian encoded integers.}
    {- UTF-32 is either UTF-32BE or UTF-32LE, using the same
       byte order mark mechanism as UTF-16, looking at the four initial
       bytes of the data stream.}}

    The cost of using one representation over the other depends on the
    character usage. For example UTF-8 is fine for latin scripts but
    wasteful for east-asian scripts, while the converse is true for UTF-16.
    I never saw any usage of UTF-32 on disk or wires, it is very wasteful.
    However, in memory, UTF-32 has the advantage that characters become
    directly indexable.

    For more information see the
    {{:http://www.unicode.org/faq/utf_bom.html}Unicode UTF-8, UTF-16,
    UTF-32 and BOM FAQ}.

    {2 Interlude — Useful scalar values}

    The following scalar values are useful to know:
    {ul
    {- U+FEFF, the byte order mark (BOM) character used to
     detect endiannes on byte order sensitive UTFs.}
    {- U+FFFD, the replacement character. Can be used to: stand
      for unrepresentable characters when transcoding from
      another representation, indicate that something
      was lost in best-effort UTF decoders, etc.}
    {- U+1F42B, the emoji bactrian camel (🐫, since Unicode 6.0.0).}}

    {2:equivalence Equivalence and normalization}

    We mentioned above that concrete textual data may be represented by
    more than one sequence of scalar values. Latin letters with
    diacritics are a simple example of that. In order to be able to
    test two sequences of scalar values for equality we should be able
    to ignore these differences. The easiest way to do so is to convert
    them to a normal form where these differences are removed and then
    use binary equality to test them.

    However first we need to define a notion of equality between
    sequences. Unicode defines two of them, which one to use depends
    on your processing context.
    {ul
    {- {e Canonical} equivalence. Equivalent
       sequences should display and and be interpreted the
       same way when printed. For example the sequence
       "B", "Ä" (<U+0042, U+00C4>) is
       canonically equivalent to "B", "A", "¨" (<U+0042, U+0041, U+0308>).}
    {- {e Compatibility} equivalence. Equivalent sequences
       may have format differences in display and may be interpreted
       differently in some contexts. For example the sequence made
       of the latin small ligature fi "ﬁ" (<U+FB01>) is compatibility
       equivalent to the sequence "f", "i" (<U+0066, U+0069>). These
       two sequences are however not canonically equivalent.}}

    Canonical equivalence is included in compatiblity equivalence: two
    canonically equivalent sequences are also compatibility
    equivalent, but the converse may not be true.

    A normal form is a function mapping a sequence of scalar values to
    a sequence of scalar values. The Unicode standard defines four
    different normal forms, the one to use depends on the equivalence
    you want and your processing context:
    {ul
    {- Normalization form D (NFD). Removes any canonical difference
       and decomposes characters. For example the sequence "é"
       (<U+00E9>) will normalize to the sequence "e", "´" (<U+0065,
       U+0301>.)}
    {- Normalization form C (NFC). Removes any canonical difference
       and composes characters. For example the sequence "e", "´"
       (<U+0065, U+0301>) will normalize to the sequence "é"
       (<U+00E9>)}
    {- Normalization form KD (NFKD). Removes canonical and compatibility
       differences and decomposes characters.}
    {- Normalization form KC (NFKC). Removes canonical and compatibility
       differences and composes characters.}}

    Once you have two sequences in a known normal form you can compare
    them using binary equality. If the normal form is NFD or NFC,
    binary equality will entail canonical equivalence of the
    sequences. If the normal form is NFKC or NFKD equality will entail
    compatibility equivalence of the sequences. Note that normal forms
    are {b not} closed under concatenation: if you concatenate two
    sequence of scalar values you have to renormalize the result.

    For more information about normalization, see the
    {{:http://www.unicode.org/faq/normalization.html}Normalization FAQ}.

    {2:collation Collation — sorting in alphabetical order}

    Normalisation forms allow to define a total order between
    sequences of scalar values using binary comparison. However this
    order is purely arbitrary. It has no meaning because the magnitude
    of a scalar value has, in general, no meaning. The process of
    ordering sequences of scalar values in a standard order like
    alphabetical order is called {e collation}. Unicode defines a
    customizable algorithm to order two sequences of scalar values in
    a meaningful way, the Unicode collation algorithm. For more
    information and further pointers see the
    {{:http://www.unicode.org/faq/collation.html}Unicode Collation
    FAQ}.

    {1:tips Biased tips for OCaml programs and libraries}

    {ul
    {- {!uchartype}}
    {- {!utf_8_strings}}
    {- {!utf_8_ascii}}
    {- {!eqcmpnorm}}
    {- {!boundaries}}
    {- {!readline}}
    {- {!alphasort}}
    {- {!noranges}}
    {- {!transcode}}
    {- {!ppcp}}
    {- {!ocamllibs}}}

    {2:uchartype Unicode characters (scalar values) as {!Uchar.t} values.}

    Since OCaml 4.03 the standard library defines the {!Uchar.t} type
    which represents {{!characters}Unicode scalar values}. Support for
    previous OCaml versions is provided by the
    {{:https://github.com/ocaml/uchar}[uchar]} OPAM/ocamlfind compatibility
    package.

    {2:utf_8_strings Unicode text as UTF-8 encoded OCaml strings}

    For most OCaml programs it will be entirely sufficient to deal
    with Unicode by just treating the byte sequence of regular OCaml
    [string]s as {b valid} UTF-8 encoded data.

    Many libraries will already return Unicode text under this
    representation. Besides latin1 identifiers having been deprecated
    in OCaml 4.01, UTF-8 encoding your sources allows you to write
    UTF-8 encoded string literals directly in your programs. Be aware
    though that as far as OCaml's compiler is concerned these are just
    sequences of bytes and you can't trust these strings to be valid
    UTF-8 as they depend on how correctly your editor encodes them.
    That is you {b will need} to validate and most likely normalize them
    unless you:
    {ul
    {- Escape their valid UTF-8 bytes explicitely. For example
       ["\xF0\x9F\x90\xAB"] is the correct encoding of U+1F42B}
    {- Or use Unicode escapes (since OCaml 4.06). For example ["\u{1F42B}"]
       will UTF-8 encode the character U+1F42B in the string}}


    Checking the validity of UTF-8 strings should only be performed at
    the boundaries of your program: on your string literals, on data
    input or on the results of untrusted libraries (be careful, some
    libraries like Yojson will happily return invalid UTF-8
    strings). This allows you to only deal with valid UTF-8 throughout
    your program and avoid redundant validity checks, internally or on
    output. The following properties of UTF-8 are useful to remember:
    {ul
    {- UTF-8 validity is closed under string concatenation:
       concatenating two valid UTF-8 strings results in a valid UTF-8
       string.}
    {- Splitting a valid UTF-8 encoded string at UTF-8
       encoded US-ASCII scalar values (i.e. at any byte < 128) will
       result in valid UTF-8 encoded substrings.}}
    For checking validity or recoding the other UTF encoding schemes
    into UTF-8 encoded OCaml [strings], the {!Uutf} module can be
    used. It will also be useful if you need to fold over the scalar
    values of your UTF-8 encoded strings, or build new UTF-8 strings
    from scalar values via {!Buffer.t} values. Support for the latter
    is however present in the OCaml {!Buffer} module since OCaml 4.06.

    {2:utf_8_ascii UTF-8 and ASCII}

    As mentioned in {!serializing}, each of the 128 US-ASCII characters is
    represented by its own US-ASCII byte representation in UTF-8. So
    if you want to look for an US-ASCII character in an UTF-8 encoded
    string, you can just scan the bytes.  But beware on the nature of
    your data and the algorithm you need to implement. For example to
    detect spaces in the string, looking for the US-ASCII space U+0020
    may not be sufficient, there are a lot of other space characters
    like the no break space U+00A0 that are beyond the US-ASCII
    repertoire. Folding over the scalar values with {!Uutf} and
    checking them with {!White.is_white_space} is a better idea. Same
    holds for line breaks, see for example {!Uutf.nln} and
    {!Uutf.readlines} for more information about these issues.

    {2:eqcmpnorm Equate, compare and normalize UTF-8 encoded
     OCaml strings}

    If you understood well the above section about
    {{!equivalence}equivalence and normalization} you should realise
    that blindly comparing UTF-8 encoded OCaml strings using
    {!Pervasives.compare} won't bring you anywhere if you don't
    normalize them before. The {!Uunf} module can be used for
    that. Remember that concatenating normalized strings does {b not}
    result in a normalized string.

    Using {!Pervasives.compare} on {e normalized} UTF-8 encoded OCaml
    strings defines a total order on them that you can use with the
    {!Map} or {!Set} modules as long as you are not interested in the actual
    {e meaning} of the order.

    For case insensitive equality have a look at the
    {{!Case.caselesseq}sample code} of the {!Case} module.

    {2:alphasort Sort strings alphabetically}

    The only solution at the moment for collating strings is to use
    {{:https://github.com/yoriyuki/Camomile}Camomile} but be aware
    that it supports only Unicode 3.2 character data so don't be
    surprised if newer scripts don't order correctly.  The official
    collation data also has been significantly tweaked since then.

    {2:boundaries Find user-perceived character, word, sentence and line
     boundaries in Unicode text.}

    The {!Uuseg} module implements the
    {{:http://www.unicode.org/reports/tr29/} Unicode text segmentation
    algorithms} to find user-perceived character, word and sentence
    boundaries in Unicode text. It also provides an implementation of
    the {{:http://www.unicode.org/reports/tr14/}Unicode Line Breaking
    Algorithm} to find line breaks and line break opportunities.

    Among other things the {!Uuseg_string} module uses these
    algorithms to provide OCaml standard library {{!Format}formatters}
    for best-effort formatting of UTF-8 encoded strings.

    {2:readline Unicode readline}

    A [readline] function as mandated by the Unicode standard is available
    in {{!Uutf.readline}[Uutf]'s sample code}.

    {2:noranges Range processing}

    Forget about trying to process Unicode characters using hard coded
    ranges of scalar values like it was possible to do with
    US-ASCII. The Unicode standard is not closed, it is evolving, new
    characters are being assigned. This makes it impossible to derive
    properties based simply on their integer value or position in
    ranges of characters. That's the reason why we have the Unicode
    character database and [Uucp] to access their properties. Using
    {!White.is_white_space} will be future proof should a new
    character deemed white be added to the standard (both [Uucp] and
    your progam will need a recompile though).

    {2:transcode Transcoding}

    Transcoding from legacy encodings to Unicode may be quite
    involved, use {{:https://github.com/yoriyuki/Camomile}Camomile} if
    you need to do that.  There is however one translation that is
    very easy and direct: it is the one from ISO 8859-1 also known as
    latin1, the default encoding of OCaml [char]s. latin1 having been
    encoded in Unicode in the range of scalar values U+0000 to U+00FF
    which corresponds to latin1 code value, the translation is
    trivial, it is the identity:
{[
let char_to_scalar_value c = Char.code c
let char_of_scalar_value s =
    if s > 255 then invalid_arg "" (* can't represent *) else
    Char.chr s
]}

    {2:ppcp Pretty-printing code points in ASCII}

    ["U+%04X"] is an OCaml formatting string for printing an US-ASCII
    representation of an Unicode code point according to the
    standards' notational conventions. This is what the
    {!Fmt.Dump.uchar} formatter does for {!Uchar.t} values.

    {2:ocamllibs Writing OCaml libraries}

    If you write a library that deals with textual data, you should,
    unless technically impossible, always interact with the client of
    the library using Unicode. If there are other encodings involved
    transcode them to/from Unicode so that the client needs only to
    deal with Unicode, the burden of dealing with the encoding mess
    has to be on the library, not the client.

    In this case there is no absolute need to depend on an Unicode
    text data structure, just use {b valid} UTF-8 encoded data as
    OCaml [string]s and the standard library {!Uchar.t} type.

    Specify clearly in the documentation that all the [string]s
    returned by or given to the library must be valid UTF-8 encoded
    data. This validity contract is important for performance reasons:
    it allows both the client and the library to trust the string and
    forgo redundant validity checks. Remember that concatenating valid
    UTF-8 strings results in valid UTF-8 string. *)

(**/**)

(* Warning this is not part of the public API and subject
   to change without notice. *)

module Cmap : sig
  type 'a tree = Empty | C of int * 'a | Cn of 'a tree * 'a tree * int * 'a
  type 'a t = { default : 'a; tree : 'a tree; }
  val get : 'a t -> int -> 'a
  val of_sorted_list : 'a -> [ `C of int * 'a ] list -> 'a t
  val height : 'a t -> int
  val word_size : ('a -> int) -> 'a t -> int
  val dump :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Rmap : sig
  type 'a tree =
    | Empty
    | R of int * int * 'a
    | Rn of 'a tree * 'a tree * int * int * 'a
  type 'a t = { default : 'a; tree : 'a tree; }
  val get : 'a t -> int -> 'a
  val of_sorted_list : 'a -> [ `R of int * int * 'a ] list -> 'a t
  val height : 'a t -> int
  val word_size : ('a -> int) -> 'a t -> int
  val dump :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Tmap : sig
  type 'a t = { default : 'a; l0 : 'a array array array; }
  val nil : 'a array
  val create : 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val word_size : ('a -> int) -> 'a t -> int
  val dump :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module Tmapbool : sig
  type t = { default : bool; l0 : string array array; }
  val nil : 'a array
  val snil : string
  val create : bool -> t
  val get : t -> int -> bool
  val set : t -> int -> bool -> unit
  val word_size : t -> int
  val dump : Format.formatter -> t -> unit
end

module Tmapbyte : sig
  type t = { default : int; l0 : string array array; }
  val nil : 'a array
  val snil : string
  val create : int -> t
  val get : t -> int -> int
  val set : t -> int -> int -> unit
  val word_size : t -> int
  val dump : Format.formatter -> t -> unit
end

module Tmap4bytes : sig
  type t = { default : string; l0 : string array array; }
  val nil : 'a array
  val snil : string
  val create : string -> t
  val word_size : t -> int
  val dump : Format.formatter -> t -> unit
  val create_uint16_pair : int * int -> t
  val get_uint16_pair : t -> int -> int * int
  val set_uint16_pair : t -> int -> int * int -> unit
end

(**/**)

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
