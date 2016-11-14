(ns bartleby.bibtex
  (:refer-clojure :exclude [char comment]) ; avoid warning about parsatron overriding (char)
  (:require [clojure.string :as string]
            [the.parsatron :refer :all]
            [bartleby.parsers :refer :all]))

(def delimiter-chars (into whitespace-chars #{\, \{ \}}))

; greedily consume the input until hitting the designated token, also consuming escaped instances
(defn until-escapable [c]
  (many
    (choice
      (attempt (string (str "\\" c)))
      (token #(not= % c)))))

; (word) matches any BibTeX-valid naked identifier, like pubtype, citekey, or field key
; i.e., a contiguous string of anything but whitespace, commas, and end braces.
(defparser word []
  (let->> [chars (many1 (any-char-except-in delimiter-chars))]
    (always (apply str chars))))

; tex-chunk and tex-block are mutually recursive
(declare tex-block)

(defparser tex-chunk []
  ; (doesn't work in naked defn form, even when simplified)
  ; accept any number of things that are not (unescaped) close curly braces
  ; returns a chunk of tex, which is either a string or a single character
  (choice
    (attempt (string "\\{"))
    (attempt (string "\\}")) ; matches same thing as (string [\\ \}]) but has different result
    (any-char-except-in #{\{ \}})
    (curly-braced (tex-block))))

(defparser tex-block []
  ; always returns a single string enclosed in braces
  (let->> [chunks (many (tex-chunk))]
    (always (str \{ (apply str chunks) \}))))

(defparser simple-string []
  (let->> [chars (until-escapable \")]
    ; convert string literal to tex block format here,
    ; by unescaping quotes and escaping braces
    (let [s (apply str chars)
          raw (string/replace s #"\\\"" "\"")
          escaped (string/replace raw #"[{}]" "\\\\$0")]
      (always (str \{ escaped \})))))

(defparser number-literal []
  (let->> [chars (many1 (digit))]
    ; just like simple-string, coerce to braced format
    (always (str \{ (apply str chars) \}))))

(defparser field-value []
  ; should this handle whitespace, or should the field parser (as is currently the case)?
  (choice (between (char \{)
                   (char \})
                   (tex-block))
          (between (char \")
                   (char \")
                   (simple-string))
          ; TODO: handle string variable references
          (number-literal)))

; (field) applies while within the braces of a BibTeX entry; it matches a
; single key-value field pair and consumes the following comma, if any.
(defparser field []
  (let->> [_ whitespace
           key (word)
           _ whitespace
           _ (char \=)
           _ whitespace
           value (field-value)
           _ whitespace
           _ (maybe (char \,))]
    (always {:key key
             :value value})))

; (reference) matchs a single BibTeX entry, returning a hash
(defparser reference []
  (let->> [_ (char \@)
           pubtype (word)
           _ (char \{)
           citekey (maybe (before (char \,) (word)))
           _ whitespace
           fields (many (field))
           ; handle entries with closed by eof rather than a proper close brace,
           ; which happens quite a bit more than you'd think
           _ (either (char \}) (eof))]
    (always {:pubtype pubtype
             :citekey citekey
             :fields fields})))

(defparser comment []
  (let->> [_ (char \%)
           _ whitespace
           characters (many1 (any-char-except \newline))
           _ (char \newline)]
    (always {:comment (apply str characters)})))

(defparser bibliography []
  ; the attempt is necessary since otherwise gardenpathing due to whitespace but then hitting eof will break
  ; or maybe looking for eof would be stricter solution?
  ; nope, that doesn't work -- produces "No matching clause" error
  (many (attempt (>> whitespace
                     ; TODO: handle special commands like @preamble or @string.
                     (choice (reference)
                             (comment))))))

(def ^:dynamic *indent* "  ")

(defn comment->string
  [{:keys [comment]}]
  (str "% " comment))

(defn field->string
  [{:keys [key value]}]
  (str *indent* key \space \= \space value \, \newline))

(defn reference->string
  [{:keys [pubtype citekey fields]}]
  ; omit citekey (and the comma after) if citekey is nil
  (str \@ pubtype \{ (some-> citekey (str \,)) \newline
    (apply str (map field->string fields))
    \}))

(defn entry->string
  [entry]
  (cond
    (:comment entry) (comment->string entry)
    (:pubtype entry) (reference->string entry)
    :else (str "Unrecognized entry: " entry)))

(defn parse
  "take a string of BibTeX, return a seq of plain Clojure entry objects"
  [s]
  (run (bibliography) s))

(defn bibtex->bibtex
  [s]
  "take a string of BibTeX, return a string of BibTeX"
  (->> s parse (map entry->string) (string/join \newline)))
