(ns bartleby.language.tex
  (:refer-clojure :exclude [char read])
  (:require [clojure.string :as str]
            [the.parsatron :refer [run defparser let->> >> always attempt char choice either letter many many1 token]]
            [bartleby.language.common :refer [whitespace-chars any-char-except-in curly-braced]]))

(def ^:private accent-commands
  {\` "\u0300"
   \' "\u0301"
   \^ "\u0302"
   \" "\u0308"
   \H "\u030B"
   \~ "\u0303"
   \c "\u0327"
   \k "\u0328"
   \= "\u0304"
   \b "\u0331"
   \. "\u0307"
   \d "\u0323"
   \r "\u030A"
   \u "\u0306"
   \v "\u030C"})
(defn accent
  "Reads a TeX accent macro (like \\'a or \\\"{o}) and converts it to a proper
  Unicode string (like á or ö) using a combining character"
  ; TODO: handle multiple accents like \~\i, \~{oo}, \'\i, and \'{\c{c}}
  []
  (let->> [_ (char \\)
           command (token accent-commands)
           body (either (letter) (curly-braced (letter)))]
    ; the combining character goes after the character it modifies
    (always (str body (accent-commands command)))))

(def ^:private fancy-character-commands
  {\l "\u0142"   ; l with stroke: ł
   \o "\u00F8"   ; o with stroke: ø
   \i "\u0131"   ; dotless i: ı
   \j "\u0237"}) ; dotless j: ȷ
(defn fancy-character
  "Read a TeX special character macro (like \\i or \\o) and convert it to an
  equivalent Unicode string (like ı or ø)"
  []
  (let->> [_ (char \\)
           command (token fancy-character-commands)]
    ; the fancy character command might have an empty argument (e.g., \i{})
    ; which will just be appended as an empty string by the next tex-block handler
    (always (fancy-character-commands command))))

(def ^:private tex-delimiters #{\# \$ \% \& \_ \{ \} \~ \@ \space})
(defn escaped-literal
  "Read an escaped TeX character and return the literal string"
  ; TeX's special characters are:
  ;     # $ % & \ ^ _ { } ~
  ; \^ is a valid command, for circumflex accents.
  ; Otherwise, these characters, and a few others that are also special,
  ; can be escaped and pass through directly.
  []
  (let->> [_ (char \\)
           c (token tex-delimiters)]
    (always c)))

(defn macro
  "Read a LaTeX macro/command (like \\emph or \\bf) as a Keyword. This
  should come after the fancy-character parser, which handles a subset of
  commands that can be represented easily in Unicode."
  []
  (let->> [_ (char \\)
           name (many1 (letter))
           _ (many (token whitespace-chars))]
    (always (keyword (str/join name)))))

(defn plain-string
  "Read a plain string of non-macro, non-group characters"
  []
  (let->> [chars (many1 (any-char-except-in #{\\ \{ \}}))]
    (always (str/join chars))))

; Return a TeX tree string built by parsing and then generating TeX strings
; in nested parsers, potentially recursively.
(defparser node []
  (choice
    (attempt (accent)) ; returns string
    (attempt (fancy-character)) ; returns string
    (attempt (macro)) ; returns nil
    ; discard escaped hyphens (hyphenation hints)
    (attempt (>> (char \\) (char \-) (always nil))) ; returns nil
    ; convert \\ to newline
    (attempt (>> (char \\) (char \\) (always \newline)))
    (attempt (escaped-literal)) ; returns string
    ; TODO: handle other escaped things?
    ; parses a TeX group, starting with an opening curly brace, {,
    ; recursing as needed, until it hits the matching closing brace, }.
    ; It's called group because { and } are synonyms for \bgroup and \egroup,
    ; respectively. Returns a collection.
    (attempt (curly-braced (many (node))))
    ; parse a sequence of anything except slashes or braces
    (plain-string)))

(defn read
  "Parse and simplify the TeX reader into a string of TeX"
  [reader]
  (run (many (node)) reader))

(defn read-str
  "Parse and simplify a TeX string into a simplified string of TeX"
  [string]
  (read string))

;;; TEX transformations

(defprotocol Flattenable
  "Flatten the list of nodes, removing commands and blocks throughout"
  (-flatten [this] "Flatten this node into a string, or nil"))

(extend-protocol Flattenable
  clojure.lang.Keyword
  (-flatten [this] nil)
  clojure.lang.Sequential
  (-flatten [this] (str/join (map -flatten this)))
  Character
  (-flatten [this] this)
  String
  (-flatten [this] this)
  nil
  (-flatten [this] nil))

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
