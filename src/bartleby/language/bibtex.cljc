(ns bartleby.language.bibtex
  (:refer-clojure :exclude [char comment read]) ; avoid warning about parsatron overriding (char)
  (:require [clojure.string :as str]
            [the.parsatron :refer :all]
            [bartleby.bibliography :refer [->Field ->Reference ->Gloss]]
            [bartleby.language.common :refer [whitespace-chars whitespace linebreak
                                              maybe any-char-except-in any-char-except
                                              curly-braced double-quoted before run-seq]]
            [bartleby.language.tex :as tex])
  (:import (bartleby.bibliography Field Reference Gloss)))

;;; BIBTEX READER

(def ^:private delimiter-chars (into whitespace-chars #{\, \{ \} \=}))

(defn- until-escapable
  "greedily consume the input until hitting the designated token,
  also consuming escaped instances"
  [c]
  (many
    (choice
      (attempt (string (str "\\" c)))
      (token #(not= % c)))))

(defn word
  "Match any BibTeX-valid naked identifier, like pubtype, citekey, or field key,
  i.e., a contiguous string of anything but whitespace, commas, and end braces."
  []
  (let->> [chars (many1 (any-char-except-in delimiter-chars))]
    (always (str/join chars))))

(defn simple-string
  "Read a simple string literal and convert to TeX syntax"
  []
  (let->> [chars (until-escapable \")]
    (let [s (str/join chars)
          ; unescaped quotes
          raw (str/replace s #"\\\"" "\"")
          ; escaped braces
          escaped (str/replace raw #"[{}]" "\\\\$0")]
      (always escaped))))

(defn number-literal
  "Consume one or more digits and return a string in TeX syntax (in curly braces)"
  []
  (let->> [chars (many1 (digit))]
    (always (str/join chars))))

(defn field-value
  "Read the RHS of a field-value field pair, as a string"
  []
  ; should this handle whitespace, or should the field parser (as is currently the case)?
  (choice (curly-braced (many (tex/tex-token)))
          (double-quoted (simple-string))
          ; TODO: handle string variable references
          (number-literal)))

(defn field
  "Use within the outer braces of a BibTeX entry, to capture a single key-value
  field pair and consume the following comma and whitespace, if any."
  []
  (let->> [key (word)
           _ whitespace
           _ (char \=)
           _ whitespace
           value (field-value)
           _ whitespace
           _ (maybe (char \,))
           _ whitespace]
    (always (->Field key value))))

(defn reference
  "Capture a single BibTeX entry, returning a Reference record"
  []
  (let->> [_ (char \@)
           pubtype (word)
           _ (char \{)
           _ whitespace
           citekey (maybe (before (>> whitespace (char \,)) (word)))
           _ whitespace
           fields (many (attempt (field)))
           ; handle entries closed by eof rather than a proper close brace,
           ; which happens quite a bit more than you'd think
           ; perhaps the lookahead wrapper isn't necessary?
           ; was: (lookahead (eof))
           _ (either (char \}) (eof))]
    (always (->Reference pubtype citekey fields))))

(defn gloss-line-characters
  "Captures a single line of interlinear comments,
  i.e., whatever is not a Reference,
  returning a sequence of characters"
  []
  (between (lookahead (any-char-except \@))
           linebreak
           (many (any-char-except-in #{\return \newline}))))

(defn gloss-line
  ; TODO: create a parsastron helper to map a parser's cok/eok values through the given function and merge this wrapper with gloss-line-characters
  []
  (let->> [characters (gloss-line-characters)]
    (always (str/join characters))))

(defn gloss
  "Capture inter-entry content in BibTeX, returning a Gloss record"
  []
  (let->> [lines (many1 (gloss-line))]
    (always (->Gloss lines))))

(defn item
  "Capture any valid bibtex item, potentially preceded by whitespace"
  []
  ; reference and gloss very intentionally have no shareable prefix, so no (attempt ...) is needed
  (>> whitespace
      ; TODO: handle special commands like @preamble or @string.
      (choice (reference)
              (gloss))))

; similar to clojure.data.json (https://clojure.github.io/data.json/),
; the primary API consists of the functions read(-str) and write(-str)

(defn read
  "Reads a single bibliography item from the BibTeX reader"
  [reader]
  (run (item) reader))

(defn read-str
  "Reads one bibliography item from the BibTeX string"
  [string]
  ; TODO: convert s to decomplected (generic b/w clj and cljs) reader first?
  (read string))

(defn read-all
  "Read input to the end, returning all bibliography items."
  [input]
  (run-seq (item)
           (>> whitespace (eof)) input))

;;; BIBTEX WRITER

(def ^{:dynamic true :private true} *indentation*)
(def ^{:dynamic true :private true} *trailing-comma?*)
(def ^{:dynamic true :private true} *trailing-newline?*)
(def ^{:dynamic true :private true} *=-padded?*)

(defprotocol BibTeXFormatter
  "Handle formatting of bibliography items into BibTeX strings"
  (-format [this] "Format this as a BibTeX string"))

(extend-protocol BibTeXFormatter
  Field
  (-format [{:keys [key value]}]
    (str *indentation* key (when *=-padded?* \space) \= (when *=-padded?* \space) (tex/-format value)))
  Reference
  (-format [{:keys [pubtype citekey fields]}]
    ; omit citekey (and the comma after) if citekey is nil
    (str \@ pubtype \{ (some-> citekey (str \,)) \newline
         (str/join (str \, \newline) (map -format fields))
         (when *trailing-comma?* \,) (when *trailing-newline?* \newline)
         \} \newline))
  Gloss
  (-format [{:keys [lines]}]
    (str/join \newline lines)))

(defn write-str
  "Convert bibliography item into BibTeX-formatted string"
  [item & options]
  (let [{:keys [indentation trailing-comma? trailing-newline? =-padded?]
         :or   {indentation       "  "
                trailing-comma?   true
                trailing-newline? true
                =-padded?         true}} options]
    (binding [*indentation*       indentation
              *trailing-comma?*   trailing-comma?
              *trailing-newline?* trailing-newline?
              *=-padded?*         =-padded?]
      (-format item))))

(defn write
  "Write BibTeX-formatted output to a java.io.Writer."
  [item ^java.io.Writer writer & options]
  (.write writer ^String (apply write-str item options)))
