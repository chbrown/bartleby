(ns bartleby.util
  "Internal API for shared functions, like name manipulation"
  (:require [clojure.string :as str]))

(defn blank?
  "Like str/blank? but also works for single characters.
  Returns true iff `x` is any of:
  * nil
  * a single whitespace character, represented as a char/java.lang.Character
  * a single whitespace codePoint, represented as an int
  * the empty string (or empty CharSequence)
  * a string (or CharSequence) of all whitespace"
  [x]
  (or (nil? x)
      (and (char?    x) (Character/isWhitespace ^char x))
      ; TODO: convert long or BigInt to int
      (and (integer? x) (Character/isWhitespace ^int x))
      ; java.lang.String implements java.lang.CharSequence
      (and (string?  x) (str/blank? x))))

(defn collapse-space
  "Replace all sequences of whitespace in s with a single space"
  [s]
  (str/replace s #"\s+" " "))

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

(defn dedupe-while
  "Similar to (clojure.core/dedupe) but removes duplicate consecutive items
  from coll for which (f item) returns true.
  Returns a transducer when no collection is provided."
  ([f]
   (fn [xf]
     (let [*inside? (volatile! false)]
       (fn
         ([] (xf))
         ([result] (xf result))
         ([result input]
          ; inside? | current? | input action | set inside?
          ; true    | true     | ignore       |
          ; true    | false    | use          | set to false
          ; false   | true     | use          | set to true
          ; false   | false    | use          |
          (let [inside? @*inside?
                current? (f input)]
            (when (not= inside? current?)
              (vreset! *inside? current?))
            (if (and inside? current?)
              result
              (xf result input))))))))
  ([f coll] (sequence (dedupe-while f) coll)))
