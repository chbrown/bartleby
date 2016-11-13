(ns bartleby.bibtex
  (:refer-clojure :exclude [char comment]) ; avoid warning about parsatron overriding (char)
  (:require [clojure.string :as string]
            [the.parsatron :refer :all]
            [clojure.set :as set]))

(def whitespace-chars #{\space \newline \tab})
(def delimiter-chars (into whitespace-chars #{\, \{ \}}))

(def whitespace (many (token whitespace-chars)))

(defn maybe [p]
  ; not sure if the "attempt" is necessary, but it seems like attempt really ought
  ; to be the default in parsatron: https://github.com/youngnh/parsatron/issues/15
  ; the README reads: "offers infinite lookahead", but fails to mention that that
  ; offer is only valid when wrapped in an (attempt ...)
  (either (attempt p) (always nil)))

(defn any-char-except [x]
  (token #(and (char? %) (not (contains? x %)))))

; greedily consume the input until hitting the designated token, also consuming escaped instances
(defn until-escapable [c]
  (many
    (choice
      (string (str "\\" c))
      (token #(not= % c)))))

; (word) matches any BibTeX-valid naked identifier, like pubtype, citekey, or field key
(defn word []
  (many1 (any-char-except delimiter-chars)))

; tex-chunk and tex-block are mutually recursive
(declare tex-block)

(defparser tex-chunk []
  ; (doesn't work in naked defn form, even when simplified)
  ; accept any number of things that are not (unescaped) close curly braces
  ; returns a chunk of tex, which is either a string or a single character
  (choice
    (string "\\}") ; matches same thing as (string [\\ \}]) but has different result
    (any-char-except #{\{ \}})
    (between (char \{)
             (char \})
             (tex-block))))

(defparser tex-block []
  ; always returns a single string enclosed in braces
  (let->> [chunks (many (tex-chunk))]
    (always (str \{ (apply str chunks) \}))))

(defparser simple-string []
  (let->> [chars (until-escapable \")]
    ; maybe convert string literal to tex block format here,
    ; by unescaping quotes and escaping braces?
    (always (str \" (apply str chars) \"))))

(defparser number-literal []
  (let->> [chars (many1 (digit))]
    (always (apply str chars))))

(defparser field-value []
  (choice (between (char \{)
                   (char \})
                   (tex-block))
          (between (char \")
                   (char \")
                   (simple-string))
          ; TODO: handle string variable references
          (number-literal)))

; (field) matches a single key-value field pair and consumes the following comma, if any
(defparser field []
  (let->> [_ whitespace
           key (word)
           _ whitespace
           _ (char \=)
           _ whitespace
           value (field-value)
           _ whitespace
           _ (maybe (char \,))]
    (always {:key (apply str key)
             :value value})))

; single BibTeX entry, a "reference"
(defparser reference []
  (let->> [_ (char \@)
           pubtype (word)
           _ (char \{)
           citekey (word)
           _ (char \,)
           _ whitespace
           fields (many (field))
           _ (char \})]
    (always {:pubtype (apply str pubtype)
             :citekey (apply str citekey)
             :fields fields})))

(defparser comment []
  (let->> [_ (char \%)
           _ whitespace
           characters (many1 (any-char-except #{\newline}))
           _ (char \newline)]
    (always {:comment (apply str characters)})))

(defparser bibliography []
  ; the attempt is necessary since otherwise gardenpathing due to whitespace but then hitting eof will break
  ; or maybe looking for eof would be stricter solution?
  ; nope, that doesn't work -- produces "No matching clause" error
  (many (attempt (>> whitespace
                     (choice (reference)
                             (comment))))))

(defn parse-single
  [s]
  (println s "TODO"))

(defn parse
  [s]
  (println s "TODO"))
