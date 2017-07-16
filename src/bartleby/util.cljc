(ns bartleby.util
  "Internal API for shared functions, like name manipulation"
  (:require [clojure.string :as str]))

(defn collapse-space
  "Replace all sequences of whitespace in s with a single space"
  [s]
  (str/replace s #"\s+" " "))

(defn map-values
  "Contruct a new map with all the values of the map kvs passed through f"
  [f kvs]
  (into {} (for [[k v] kvs] [k (f v)])))

(defn normalize-nfc
  "NFC-normalize the string s"
  [s]
  #?(:clj (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

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
