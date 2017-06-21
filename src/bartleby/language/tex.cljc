(ns bartleby.language.tex
  (:refer-clojure :exclude [char read flatten])
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [the.parsatron :refer [run defparser let->> >> always attempt bind between choice either many many1
                                   token any-char char letter letter?]]
            [bartleby.language.common :refer [whitespace whitespace-chars any-char-except]]))

(defn- not-letter
  "Consume a non-letter [^a-zA-Z] character."
  []
  (token (complement letter?)))

(defn- fmap
  "Run the parser `p` and run the result through `f`"
  [f p]
  (bind p #(always (f %))))

(defn control-symbol
  "Parse a TeX 'control symbol', i.e., the escape character + another non-letter character.
  Subsequent whitespace is not consumed. Returns a string."
  []
  (>> (char \\) ; preceded by /
      (fmap str (not-letter))))

(defn control-word
  "Parse a TeX 'control word', i.e., the escape character + a sequence of letter characters.
  All whitespace after the control word is immediately absorbed. Returns a string."
  []
  (between (char \\) ; preceded by /
           whitespace ; followed by whitespace
           (fmap str/join (many1 (letter)))))

(defn control-sequence
  "Parse a TeX 'control sequence', which can be a 'control symbol' or a 'control word'.
  Returns a Keyword."
  []
  (fmap keyword (choice (attempt (control-symbol)) (control-word))))

(defn tex-comment
  "Parse a TeX comment. For now, immediately discard the contents, returning only the empty string."
  []
  ; TODO: handle other linebreaks
  (>> (char \%) (many (any-char-except \newline)) (char \newline) (always "")))

; Return a TeX tree string built by parsing and then generating TeX strings
; in nested parsers, potentially recursively.
(defparser tex-token []
  (choice
    (attempt (control-sequence))
    ; parse a TeX 'group', starting with an opening curly brace, {,
    ; recursing as needed, until it hits the matching closing brace, }.
    ; { and } are synonyms for \bgroup and \egroup, respectively.
    ; returns a seq of sub-nodes
    ; Nb. This is not its own parser since it's recursive and otherwise we
    ;     would need to (declare tex-token) before (defn tex-group).
    (attempt (between (char \{) (char \}) (many (tex-token))))
    (tex-comment)
    ; TODO: handle other escaped things?
    ; parse anything (and everything) else as a raw character, except for },
    ; which we have to fail on so that groups can parse it
    (any-char-except \})))

(defn read
  "Parse and simplify the TeX reader into a string of TeX"
  [reader]
  (run (many (tex-token)) reader))

(defn read-str
  "Parse and simplify a TeX string into a simplified string of TeX"
  [string]
  (read string))

;;; TEX transformations

;; zipper helpers

(defn- blank?
  "Like str/blank? but also works for single characters"
  [x]
  (or (nil? x)
      (and (char? x) (Character/isWhitespace ^Character x))
      (and (string? x) (str/blank? x))))

(defn- loc-blank?
  [loc]
  (when loc
    ; it's illegal to call zip/node on the end? loc, so we have to check for that first
    (when-not (zip/end? loc)
      (blank? (zip/node loc)))))

(defn- right-while
  "Go right over nodes until (pred loc) returns false; returns the first loc for
  which pred returns false, so may return the given loc. Returns nil if there are
  no siblings to the right for which (pred loc) returns false."
  [loc pred]
  (if (pred loc)
    (right-while (zip/right loc) pred)
    loc))

(defn- zip-walk
  "Run `f` on each loc in `tree` (a seq), allowing `f` to modify the loc as
  much as wanted / needed."
  [f tree]
  {:pre [#(fn? f) #(seq? tree)]}
  (loop [loc (zip/seq-zip tree)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (f loc))))))

(defn- token->control-char
  "If `token` (a parsed TeX token) is a control symbol or a 1-letter control word,
  return its value as a Character. Otherwise, if it's not a control sequence,
  or is a control word longer than 1-character, return nil."
  [token]
  (when (keyword? token)
    (let [s (name token)]
      (when (= 1 (count s))
        (.charAt s 0)))))

;; actual transformation functions

; interpret-character-commands
; (Knuth calls them "control sequences", but Lamport calls them "commands",
; which is shorter, so we'll go with "command")

(def ^:private command->character
  {\- nil ; discard escaped hyphens (hyphenation hints)
   \l \ł
   \o \ø
   \i \ı
   \j \ȷ
   \\ \newline
   \# \#
   \$ \$
   \% \%
   \& \&
   \_ \_
   \{ \{
   \} \}
   \@ \@})

(defn interpret-character-commands
  "Replace each control sequence in a TeX tree that represents a specific
  character with that character."
  [tree]
  (-> (fn [loc]
        ; take a zipper loc, and if it looks like a command we should replace, do so,
        ; otherwise return the given loc, unchanged
        (let [control-char (-> loc zip/node token->control-char)]
          ; we have to check contains? since \- returns nil
          (if (contains? command->character control-char)
            (zip/replace loc (get command->character control-char))
            loc)))
      (zip-walk tree)))

; interpret-accent-commands

(def ^:private accent-command->combining-character
  "Mapping of TeX accent macros (like \\' and \\\") to the corresponding
  Unicode combining character (like ´ and ¨)."
  ; the combining character goes after the character it modifies
  {\` \u0300
   \' \u0301
   \^ \u0302
   \" \u0308
   \H \u030B
   \~ \u0303
   \c \u0327
   \k \u0328
   \= \u0304
   \b \u0331
   \. \u0307
   \d \u0323
   \r \u030A
   \u \u0306
   \v \u030C})

(defn interpret-accent-commands
  "Unescape all accents in TeX node tree.
  If loc points to an accent command, remove it and insert the appropriate
  combining character after the following (non-whitespace) token
  Take a node in the TeX tree and if it's an accent command (1-character keyword)
  that has a corresponding combining character, return that character."
  [tree]
  (-> (fn [loc]
        (if-let [combining-character (-> loc
                                         zip/node
                                         token->control-char
                                         accent-command->combining-character)]
          ; iff we've got an accent command that we recognize...
          (let [; Remove the accent command
                loc (zip/replace loc nil)
                ; and jump over subsequent space (if possible)
                next-loc (or (right-while loc loc-blank?) loc)]
            ; Two possible cases after we've skipped over any space:
            (if (zip/branch? next-loc)
              ; 1. The next-loc node is a branch, in which case we go into it
              ;    and if there's anything in it at the root level before any nesting, combine with that
              (if-let [sub-loc (-> next-loc zip/down (right-while loc-blank?))]
                (if (zip/branch? sub-loc)
                  ; no luck, but might as well stick it here.
                  (-> sub-loc
                      (zip/insert-left combining-character)
                      zip/up)
                  ; cool, we found somewhere to put it!
                  (-> sub-loc
                      (zip/insert-right combining-character)
                      zip/up))
                ; empty group :(
                (-> next-loc
                    (zip/insert-right combining-character)))
              ; 2. next-loc is not a branch, so we can just combine with whatever next-loc is.
              (-> next-loc
                  (zip/insert-right combining-character)
                  zip/prev)))
          ; not a character to escape; pass-through:
          loc))
      (zip-walk tree)))

; flatten

(defn flatten
  "Flatten the list of nodes, removing commands and blocks throughout;
  flatten this node into a string, or nil.
  Flatten the node at loc, removing it if it's empty, replacing it with its
  children if there are any, and removing any commands outright."
  [tree]
  (-> (fn [loc]
        ; only flatten if loc is a branch AND loc is not the root
        ;   (zip/path root-loc) returns an empty list
        (if (and (zip/branch? loc) (seq (zip/path loc)))
          (zip/remove (reduce zip/insert-right loc (reverse (zip/children loc))))
          ; or if it's a command, remove it
          (if (keyword? (zip/node loc))
            (-> loc zip/remove)
            loc)))
      (zip-walk tree)))

; transformation pipeline

(defn simplify
  "Replace fancy characters, then unescape accents, then flatten"
  [tree]
  (-> tree interpret-character-commands interpret-accent-commands flatten))

;;; TEX WRITER

(defprotocol TeXFormatter
  "Handle formatting of a TeX node tree into TeX strings"
  (-format [this] "Format this as a TeX string"))

(extend-protocol TeXFormatter
  clojure.lang.Keyword ; for macros
  (-format [this] (str \\ (name this)))
  clojure.lang.Sequential
  (-format [this] (str \{ (str/join (map -format this)) \}))
  Character
  (-format [this] this)
  String
  (-format [this] this)
  nil
  (-format [this] nil))

(defn write-str
  "Convert TeX tree into TeX-formatted string"
  [tree & options]
  (str/join (map -format tree)))

(defn write
  "Write TeX-formatted output to a java.io.Writer."
  [tree ^java.io.Writer writer & options]
  (.write writer ^String (apply write-str tree options)))
