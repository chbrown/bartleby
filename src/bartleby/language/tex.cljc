(ns bartleby.language.tex
  "This module is for parsing strings of (La)TeX code into a tree of data structures.

  There are several kinds of TeX nodes/trees/tokens:
  1. Character: which is the form of the input, read character-by-character
  2. String: which can always be treated as a flattened out sequence of characters
  3. nil: indicating a gap, which should be considered equivalent to the empty string, and thus, vacuous
  4. Group: a special record wrapping a collection, indicating that the contents are to be treated as a TeX 'group'
  5. ControlSequence: a special record with a single field:
     * `name`: a string (a single non-letter for control symbols)
  6. Sequential: a seq of TeX nodes (or empty, equivalent to nil)"
  (:refer-clojure :exclude [char read])
  (:require [clojure.string :as str]
            [bartleby.util :refer [blank?]]
            [the.parsatron :refer [run defparser let->>
                                   always attempt
                                   >> nxt bind
                                   choice either
                                   many many1 times
                                   token char letter?]]))

(defrecord Group [coll])

(defrecord ControlSequence [name])

;; constants

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

;; predicates

(defn- accent-command?
  "A command is an accent command if it matches one of the known accent command strings"
  [command-name]
  (contains? accent-command->combining-character command-name))

(defn- control-word?
  "A command is a control word if it is longer than one character or is a single letter (not a symbol)"
  [command-name]
  (or (> (count command-name) 1)
      (letter? (first command-name))))

;; parsers

(defn- space
  "Consume a single whitespace character"
  []
  (token whitespace-characters))

(defn control-sequence
  "Parse a TeX 'control sequence', which can be a 'control symbol' or a 'control word'.
  Knuth calls them 'control sequences', but Lamport calls them 'commands'.

  - A 'control symbol' is the escape character + another non-letter character.
    Subsequent whitespace is not consumed. Returns a string.
    - One exception is that accent commands ALWAYS absorb space directly after them.
      * In the simplest case, they are followed by a character, and they attach to that character.
      * If they are followed by a block, things get tricky, as they are not allowed to attach to any leading space in the block.
  - A 'control word', i.e., the escape character + a sequence of letter characters.
    All whitespace after the control word is immediately absorbed.

  This parser produces a ControlSequence instance."
  []
  (>> (char \\)
      ; this is a bit tricky since accent commands absorb the space after them,
      ; but some accent command share the same prefix with control words.
      ; further, control-symbol would match some of the accent characters
      (bind (choice (times 1 (token (complement letter?))) ; a single non-letter [^a-zA-Z] character
                    (many1 (token letter?)))
            (fn [characters]
              (let [command-name (str/join characters)]
                ; if command-name is an accent command or a control word...
                (if (or (accent-command? command-name) (control-word? command-name))
                  ; ...absorb subsequent whitespace
                  (>> (many (space)) (always (ControlSequence. command-name)))
                  ; otherwise, after non-accent control symbols, don't absorb whitespace
                  (always (ControlSequence. command-name))))))))

(defn tex-comment
  "Parse a TeX comment. For now, immediately discard the contents and produce nil."
  []
  (>> (char \%)
      ; TODO: handle other linebreaks
      (many (token #(not= % \newline)))
      (char \newline)
      (always nil)))

(defn dash-ligature
  "Parse a dash ligature; i.e., one of: -, --, ---"
  []
  (>> (char \-) (choice
                  (>> (char \-) (choice
                                  (>> (char \-) (always (ControlSequence. "textemdash")))
                                  (always (ControlSequence. "textendash"))))
                  (always \-))))

(defn quoteleft-ligature
  "Parse a left-quote ligature; i.e., one of: `, ``"
  []
  (>> (char \`) (choice
                  (>> (char \`) (always (ControlSequence. "textquotedblleft")))
                  (always (ControlSequence. "textquoteleft")))))

(defn quoteright-ligature
  "Parse a right-quote ligature; i.e., one of: ', ''"
  []
  (>> (char \') (choice
                  (>> (char \') (always (ControlSequence. "textquotedblright")))
                  (always (ControlSequence. "textquoteright")))))

; tex-token and group are mutually recursive, so one has to be declared before the other uses it
(declare tex-token)

(defn group
  "parse a TeX 'group', starting with an opening curly brace, {,
  recursing as needed, until it hits the matching closing brace, }.
  { and } are synonyms for \\bgroup and \\egroup, respectively.
  returns a Group instance, whose coll is a list of tex tokens,
  or the empty vector, [], if the group is empty,
  because parsatron/many returns [] for 0 matches rather than nil."
  []
  (let->> [_ (char \{)
           tokens (many (tex-token))
           _ (char \})]
    (always (Group. tokens))))

; Return a TeX tree string built by parsing and then generating TeX strings
; in nested parsers, potentially recursively.
(defparser tex-token []
  (choice
    ; we cannot combine control-sequences (aka. commands) with their arguments now,
    ; since the latex compiler is eager, but the parser is patient;
    ; i.e. parsing \textbf\'a bolds the stranded acute accent, not an acute-accented "a"
    (control-sequence)
    (attempt (group)) ; maybe needn't be attempt?
    ; (tex-comment) ; bibtex uses percent-encoded URLs but this should be enabled in non-bibtex contexts
    ; Handle ligatures: plain characters that get tranformed into non-ASCII
    ; when rendered; see Chapter 9 of The TeX Book, page 51.
    ; --- and -- and -
    (dash-ligature)
    ; TODO: maybe handle quotes differently somehow in math environments?
    ; `` and `
    (quoteleft-ligature)
    ; '' and '
    (quoteright-ligature)
    (attempt (>> (char \!) (char \`) (always (ControlSequence. "textexclamdown"))))
    (attempt (>> (char \?) (char \`) (always (ControlSequence. "textquestiondown"))))
    ; not technically a ligature:
    (>> (char \~) (always \u00A0)) ; NO-BREAK SPACE (U+00A0)
    ; TODO: handle other escaped things?
    ; parse anything (and everything) else as a raw character, except for },
    ; which we have to fail on so that groups can parse it
    (token #(not= % \}))))

; (defn read
;   "Parse and simplify the TeX reader into a string of TeX"
;   [reader]
;   (run (many (tex-token)) reader))

(defn read-str
  "Parse a TeX string into a sequence of TeX tokens."
  [string]
  (run (many (tex-token)) string))

;; writer

(defprotocol TeXWriter
  "Format TeX nodes"
  (write-str [this] ; & _options
    "Format the TeX data structure, `this`, returning a string."))

(extend-protocol TeXWriter
  clojure.lang.Sequential
  (write-str [coll]
    (str/join (map write-str coll)))
  Group
  (write-str [group]
    (str \{ (write-str (:coll group)) \}))
  ControlSequence
  (write-str [control-sequence]
    (str \\ (:name control-sequence)))
  Character
  (write-str [character]
    character)
  String
  (write-str [string]
    string)
  nil
  (write-str [_]
    nil))

(defn write
  "Write TeX-formatted output to a java.io.Writer."
  [tree ^java.io.Writer writer & options]
  (.write writer ^String (apply write-str tree options)))

;; manipulations

(defn join
  "Return a new TeX sequence that is the concatenation of the given TeX sequences,
  separated by separator (a TeX token of some sort), if given."
  ([documents]
   (apply concat documents))
  ([separator documents]
   (apply concat (interpose separator documents))))

;; interpretation

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

(defn- command->arity
  "Return the positive integer arity of the command indicated by `command-name` (a string)"
  [command-name]
  {:pre  [(string? command-name)]
   :post [(integer? %)]}
  (cond
    (contains? command->character command-name) 0
    (accent-command? command-name) 1
    ; TODO: look it up in a contextual store somehow
    :else 1))

(defn- interpret-accent-command
  "Find the combining character and put it after the argument"
  [command-name arguments]
  {:pre [(< (count arguments) 2)]} ; arguments can be nil (count => 0) or have a single item (count => 1)
  (let [combining-character (accent-command->combining-character command-name)
        argument (first arguments)]
    ; TODO: handle the edge cases here
    (if (or (nil? argument) (sequential? argument))
      (if (empty? argument)
        ; if the given argument is empty, fabricate a space to combine with
        (list \space combining-character)
        ; else, dig in to find a viable target
        (let [tokens (drop-while blank? argument)]
          (if (char? (first tokens))
            ; okay, put it after that
            (list* (first tokens) combining-character (rest tokens))
            ; otherwise, use a dummy space to stick it right here
            (list* \space combining-character tokens))))
      ; argument is a single non-nil value, probably a character
      (list argument combining-character))))

(defn- interpret-command
  [command-name arguments]
  (cond
    ; if we've got a simple command -> character substitution, do it.
    (contains? command->character command-name) (command->character command-name)
    ; interpreting accent commands is a bit tricky, so we jump off to a separate function
    (accent-command? command-name) (interpret-accent-command command-name arguments)
    ; other control symbols are replaced with their literal name
    (not (control-word? command-name)) command-name
    ; otherwise, return the interpreted arguments, and drop the command-name
    :else arguments))

(declare interpret-tokens)

(defn interpret-commands
  "Replace fancy characters, unescape accents, and flatten groups."
  [tree]
  (condp instance? tree
    ; reduce sequences via interpret-tokens
    clojure.lang.Sequential (interpret-tokens tree)
    ; extract the contents of a group (and drop the group-ness)
    Group (interpret-tokens (:coll tree))
    ; interpret a lone command; commands that are used in a proper sequence won't end up here,
    ; only commands that are a naked argument to another command (which is atypical)
    ControlSequence (interpret-command (:name tree) nil)
    ; identity for everything else (primitive values)
    tree))

(defn- interpret-tokens
  "Interpret `tokens` as a sequence of TeX tokens.
  The argument parser depends on the arity of the given command;
  e.g. \\o is 0-argument command, so {\\o} is fine.
       but since \\textbf is a 1-argument command, {\\textbf} is a parse error.
  Takes a sequence and returns a sequence.
  This processes nested values before wider/outer ones."
  [tokens]
  (lazy-seq
    (when-let [token (first tokens)]
      (if (instance? ControlSequence token)
        ; if it's a control-sequence, get its name, collect its arguments, and interpret them
        (let [command-name (:name token)
              arity (command->arity command-name)
              ; TODO: this needs to fail immediately here if (take arity ...)
              ; returns a seq of less than length arity
              argument-tokens (take arity (rest tokens))
              ; interpret each of the given arguments before we get to them
              arguments (map interpret-commands argument-tokens)]
          ; now pack together the result of the command and its arguments we just captured
          (cons (interpret-command command-name arguments)
                ; and continue with the sequence following all that
                (interpret-tokens (drop arity (rest tokens)))))
        ; otherwise we process it directly and continue on
        (cons (interpret-commands token)
              (interpret-tokens (rest tokens)))))))
