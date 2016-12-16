(ns bartleby.language.bibtex
  (:refer-clojure :exclude [char comment read]) ; avoid warning about parsatron overriding (char)
  (:require [clojure.string :as string]
            [the.parsatron :refer :all]
            [bartleby.language.common :refer :all]))

; object model (records)
; ======================

(defrecord Field [key value])

(defprotocol Indented
  (toString [this indent] "Return customized string representation"))

(defprotocol ToJSON
  (toJSON [this] "Flatten the record to a flat JSON-friendly structure"))

(defrecord Reference [pubtype citekey fields]
  Object
  (toString [this] (toString this "  "))
  Indented
  (toString [this indent]
    ; omit citekey (and the comma after) if citekey is nil
    (str \@ pubtype \{ (some-> citekey (str \,)) \newline
      (->>
        (for [{:keys [key value]} fields]
          (str indent key \space \= \space value \, \newline))
        (apply str))
      \}))
  ToJSON
  (toJSON [this]
    (into {"pubtype" pubtype, "citekey" citekey} fields)))

(defrecord Comment [comment]
  Object
  (toString [this]
    (str "% " comment))
  Indented
  (toString [this indent] (toString this))
  ToJSON
  (toJSON [this]
    {"comment" comment}))

(defn fromJSON
  [object]
  (if (contains? object "pubtype")
    ; (map->Reference object)
    (Reference. (get object "pubtype") (get object "citekey") (dissoc object "pubtype" "citekey"))
    ; (map->Comment object)
    (Comment. (get object "comment"))))

; parsing
; =======

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
    (always (apply str chars))))

; tex-chunk and tex-block are mutually recursive
(declare tex-block)

; doesn't work in naked defn form, even when simplified, due to mutual recursion;
; one of them (tex-block or tex-chunk) has to be lazy!
(defparser tex-chunk []
  ; accept any number of things that are not (unescaped) close curly braces
  ; returns a chunk of tex, which is either a string or a single character
  (choice
    (attempt (string "\\{"))
    (attempt (string "\\}")) ; matches same thing as (string [\\ \}]) but has different result
    (any-char-except-in #{\{ \}})
    (curly-braced (tex-block))))

(defn tex-block
  "Reads a block of TeX syntax, recursively, and returns a single string
  enclosed in curly braces"
  []
  (let->> [chunks (many (tex-chunk))]
    (always (str \{ (apply str chunks) \}))))

(defn simple-string
  "Read a simple string literal and convert to TeX syntax"
  []
  (let->> [chars (until-escapable \")]
    (let [s (apply str chars)
          ; unescaped quotes
          raw (string/replace s #"\\\"" "\"")
          ; escaped braces
          escaped (string/replace raw #"[{}]" "\\\\$0")]
      (always (str \{ escaped \})))))

(defn number-literal
  "Consume one or more digits and return a string in TeX syntax (in curly braces)"
  []
  (let->> [chars (many1 (digit))]
    (always (str \{ (apply str chars) \}))))

(defn field-value
  "Read the RHS of a field-value field pair, as a string"
  []
  ; should this handle whitespace, or should the field parser (as is currently the case)?
  (choice (curly-braced (tex-block))
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
    (always (Field. key value))))

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
    (always (Reference. pubtype citekey fields))))

(defn comment
  "Captures a single line of a comment, returning a Comment record"
  []
  (let->> [_ (char \%)
           _ whitespace
           characters (many1 (any-char-except \newline))
           _ (char \newline)]
    (always (Comment. (apply str characters)))))

(defparser item []
  ; the attempt is necessary since otherwise gardenpathing due to whitespace but then hitting eof will break
  ; or maybe looking for eof would be stricter solution?
  ; nope, that doesn't work -- produces "No matching clause" error
  (attempt (>> whitespace
               ; TODO: handle special commands like @preamble or @string.
               (choice (reference)
                       (comment)))))

(defparser all-items []
  (let->> [items (many (item))
           _ whitespace
           _ (eof)]
    (always items)))

; similar to clojure.data.json (http://clojure.github.io/data.json/),
; the primary API consists of the functions read and write

(defn read
  "read the first bibliography item from the given stream of BibTeX characters"
  [reader]
  (run (item) reader))

(defn read-str
  "read the first bibliography item from the given string of BibTeX"
  [s]
  ; TODO: convert s to decomplected (generic b/w clj and cljs) reader first?
  (read s))

(defn write-str
  [bibtex-record & options]
  (toString bibtex-record (get options :indent "  ")))

(defn write
  [bibtex-record writer & options]
  (.write writer (write-str bibtex-record options)))

; read-all and copy are kind of hacks until I get a better handle
; on a/the common interface around clj/cljs readers
(defn read-all
  [s]
  (run (all-items) s))
