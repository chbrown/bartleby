(ns bartleby.util
  "Internal API for shared functions, like name manipulation"
  (:require [clojure.string :as str]))

(defn blank?
  "Like str/blank? but also works for single characters.
  Returns true iff `x` is any of:
  * nil
  * a single whitespace character, represented as a char/java.lang.Character
  * the empty string (or empty CharSequence)
  * a string (or CharSequence) of all whitespace"
  [x]
  (or (nil? x)
      (and (char? x)   #?(:clj  (Character/isWhitespace ^char x)
                          :cljs (re-matches #"\s" x)))
      ; java.lang.String implements java.lang.CharSequence
      (and (string? x) (str/blank? x))))

(def ^:private collapsible-space-pattern
  "RegEx pattern that matches a stretch of whitespace that can be collapsed into
  a single space, which would be trivial except for combining marks, which this
  pattern accommodates (it lets combining characters hold onto the character
  they're combining with), and does so cross-platform (a bit of a hack for CLJS).
  * JVM: (?!\\p{Mn}) is a negative lookahead for a character in the Unicode
    Nonspacing_Mark general category
  * CLJS: JavaScript regular expressions do not support \\p{...},
    but the 0x300-0x36F range covers most of the Unicode Nonspacing_Mark (Mn) category."
  #?(:clj  #"\s+(?!\p{M})"
     :cljs #"\s+(?![\u0300-\u036F])"))

(defn collapse-space
  "Replace all sequences of whitespace in s with a single space"
  [s]
  (str/replace s collapsible-space-pattern " "))

(defn map-values
  "Contruct a new map with all the values of the map kvs passed through f"
  [f kvs]
  (into {} (for [[k v] kvs] [k (f v)])))

(defn normalize-unicode
  "NFKC-normalize the string s.
  See https://unicode.org/reports/tr15/"
  [s]
  #?(:clj (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFKC)
     :cljs (.normalize s "NFKC")))

(defn re-escape
  "Escape any regular expression syntax characters in s such that the result can be
  embedded in a regular expression, but will only match literally equivalent strings."
  [s]
  (str/replace s #"([.*+?^=!:${}()|[\\]/\\])" "\\\\$1"))

(defn split-fullname
  "Parse the string fullname into a vector of [given-names surname],
  or just [given-names] if no surname is given."
  [fullname]
  (let [[prefix suffix] (str/split fullname #"\s*,\s+" 2)]
    (if suffix
      ; handling manually comma-separated names is easy
      [suffix prefix]
      ; it's a little trickier if there was no comma
      ; TODO: handle von, da, del, etc.
      (let [parts (str/split prefix #"\s+")]
        (if (> (count parts) 2)
          [(str/join \space (butlast parts)) (last parts)]
          parts)))))

(defn author->lastnames
  "Get last names from BibTeX-formatted author string fullnames"
  [fullnames]
  (->> (str/split fullnames #"\s+and\s+")
       (map split-fullname)
       (map last)))

(defn format-names
  "Join a collection of (last-)names as they would naturally be formatted.
  [A]         ->   A
  [A, B]      ->   A and B
  [A, B, C]   ->   A, B and C (no Oxford comma)"
  [names]
  (str/join " and " (if (> (count names) 2)
                      ; group 1: take all but the final name, join those elements with a comma
                      ; group 2: simply take the final name
                      (list (str/join ", " (butlast names)) (last names))
                      names)))

(defn partition-dynamically
  "Group all the x's in `xs` into partitions based on the integer returned by (arity x)."
  [coll arity]
  {:pre [(coll? coll) (ifn? arity)]}
  (lazy-seq
    (when-let [item (first coll)]
      (let [item-arity (or (arity item) 0)
            [items more-coll] (split-at item-arity (rest coll))]
        ; TODO: do something (throw?) if `items` is shorter than `item-arity`
        (cons (list* item items)
              ; continue on with the rest
              (partition-dynamically more-coll arity))))))
