(ns bartleby.language.texdata
  (:require [clojure.set :refer [map-invert]]))

(def whitespace-characters
  "Set of the characters:
  Horizontal Tab (U+0009)
  Line Feed (U+000A)
  Vertical Tab (U+000B)
  Form Feed (U+000C)
  Carriage Return (U+000D)
  Space (U+0020)"
  #{\tab \newline \u000B \formfeed \return \space})

(def accent-command->combining-character
  "Mapping of TeX accent macros (like \\' and \\\", as well as \\v) to the corresponding
  Unicode combining character (like ´ and ¨, and ˇ).

  In Unicode, the combining character goes after the character it modifies."
  {"`"  \u0300
   "'"  \u0301
   "^"  \u0302
   "\"" \u0308
   "H"  \u030B
   "~"  \u0303
   "c"  \u0327
   "k"  \u0328
   "="  \u0304
   "b"  \u0331
   "."  \u0307
   "d"  \u0323
   "r"  \u030A
   "u"  \u0306
   "v"  \u030C
   "textcircled" \u20DD})

(def command->character
  "Mapping of zero-argument TeX commands to the single literal Unicode character they represent."
  {"relax" nil ; loosely translated :)-
   "-"  nil ; discard escaped hyphens (hyphenation hints)
   "l"  \ł
   "o"  \ø
   "i"  \ı
   "j"  \ȷ
   "\\" \newline
   "#"  \#
   "$"  \$
   "%"  \%
   "&"  \&
   "_"  \_
   "{"  \{
   "}"  \}
   "@"  \@
   "copyright"            \©
   "textcopyright"        \©
   "dag"                  \†
   "textdagger"           \†
   "ddag"                 \‡
   "textdaggerdbl"        \‡
   "guillemotleft"        \«
   "guillemotright"       \»
   "guilsinglleft"        \‹
   "guilsinglright"       \›
   "ldots"                \…
   "dots"                 \…
   "textellipsis"         \…
   "lq"                   \‘
   "P"                    \¶
   "textparagraph"        \¶
   "pounds"               \£
   "textsterling"         \£
   "quotedblbase"         \„
   "quotesinglbase"       \‚
   "rq"                   \’
   "S"                    \§
   "textasciicircum"      \^
   "textasciitilde"       \~
   "textasteriskcentered" \*
   "textbackslash"        \\
   "textbar"              \|
   "textbardbl"           \‖
   "textbigcircle"        \◯
   "textbraceleft"        \{
   "textbraceright"       \}
   "textbullet"           \•
   "textdollar"           \$
   "textemdash"           \—
   "textendash"           \–
   "texteuro"             \€
   "textexclamdown"       \¡
   "textgreater"          \>
   "textless"             \<
   "textleftarrow"        \←
   "textordfeminine"      \ª
   "textordmasculine"     \º
   "textperiodcentered"   \·
   "textquestiondown"     \¿
   "textquotedblleft"     \“
   "textquotedblright"    \”
   "textquoteleft"        \‘
   "textquoteright"       \’
   "textregistered"       \®
   "textrightarrow"       \→
   "texttrademark"        \™
   "textunderscore"       \_})

(def character->command
  "Mapping of non-nil special characters to corresponding 0-argument TeX commands"
  (-> command->character
      (map-invert)
      (dissoc nil)))

(def command->ligature
  {"textexclamdown"    "!`"
   "textquestiondown"  "?`"
   "textemdash"        "---"
   "textendash"        "--"
   "textquotedblleft"  "``"
   "textquoteleft"     "`"
   "textquotedblright" "''"
   "textquoteright"    "'"})
