(ns bartleby.language.bibtex
  (:refer-clojure :exclude [char comment read]) ; avoid warning about parsatron overriding (char)
  (:require [clojure.string :as str]
            [the.parsatron :as parsatron :refer [run let->> >> always attempt between char choice digit either eof lookahead many many1 string token]]
            [bartleby.bibliography :refer [->Field ->Reference ->Gloss]]
            [bartleby.language.tex :as tex])
  (:import (bartleby.bibliography Field Reference Gloss)))

;;; BIBTEX READER

(def ^:private delimiter-chars (into tex/whitespace-chars #{\, \{ \} \=}))

(defn- until-escapable
  "greedily consume the input until hitting the designated token,
  also consuming escaped instances"
  [c]
  (many
    (choice
      (attempt (string (str "\\" c)))
      (token #(not= % c)))))

(defn- any-char-except-in [xs]
  (token #(and (char? %) (not (contains? xs %)))))

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

(defn- maybe [p]
  ; not sure if the "attempt" is necessary, but it seems like attempt really ought
  ; to be the default in parsatron: https://github.com/youngnh/parsatron/issues/15
  ; the README reads: "offers infinite lookahead", but fails to mention that that
  ; offer is only valid when wrapped in an (attempt ...)
  (either (attempt p) (always nil)))

(defn- curly-braced [p]
  ; p, e.g., (many (any-char-except-in #{\{ \}}))
  (between (char \{) (char \}) p))

(defn- double-quoted [p]
  (between (char \") (char \") p))

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
           _ tex/whitespace
           _ (char \=)
           _ tex/whitespace
           value (field-value)
           _ tex/whitespace
           _ (maybe (char \,))
           _ tex/whitespace]
    (always (->Field key value))))

(defn- before
  "Parse p followed by close, but return the value of p;
   equivalent to (between (always nil) close p)"
  [close p]
  (let->> [x p
           _ close]
    (always x)))

(defn reference
  "Capture a single BibTeX entry, returning a Reference record"
  []
  (let->> [_ (char \@)
           pubtype (word)
           _ (char \{)
           _ tex/whitespace
           citekey (maybe (before (>> tex/whitespace (char \,)) (word)))
           _ tex/whitespace
           fields (many (attempt (field)))
           ; handle entries closed by eof rather than a proper close brace,
           ; which happens quite a bit more than you'd think
           ; perhaps the lookahead wrapper isn't necessary?
           ; was: (lookahead (eof))
           _ (either (char \}) (eof))]
    (always (->Reference pubtype citekey fields))))

(def linebreak (either (>> (char \return) (attempt (char \newline)))
                       (char \newline)))

(defn gloss-line-characters
  "Captures a single line of interlinear comments,
  i.e., whatever is not a Reference,
  returning a sequence of characters"
  []
  (between (lookahead (token #(not= % \@)))
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
  (>> tex/whitespace
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

;; run-parser-seq and run-seq that could be in parsatron upstream:

(defn- run-parser-seq
  "Similar to parsatron/run-parser"
  [p endp state]
  (letfn [(pcok [item new-state]
            (cons item (lazy-seq (run-parser-seq p endp new-state))))
          (peok [_ {:keys [pos]}]
            (throw (parsatron/fail (parsatron/show-error (parsatron/unexpect-error "that run-seq parser p would accept an empty string" pos)))))
          (perr [err-from-p]
            (letfn [(endpok [_ _]
                      nil)
                    (endperr [err-from-endp]
                      (throw (parsatron/fail (parsatron/show-error (parsatron/merge-errors err-from-p err-from-endp)))))]
              (parsatron/parsatron-poline endp state endpok endperr endpok endperr)))]
    (parsatron/parsatron-poline p state pcok perr peok perr)))

(defn- run-seq
  "Similar to parsatron/run, but repeatedly run the parser p over the input.
  If the parser produces an error, try endp.
  If endp fails, fail with both p's and endp's errors."
  [p endp input]
  (let [initial-state (parsatron/->InputState input (parsatron/->SourcePos 1 1))]
    (run-parser-seq p endp initial-state)))

(defn read-all
  "Read input to the end, returning all bibliography items."
  [input]
  (run-seq (item)
           (>> tex/whitespace (eof)) input))

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
