(ns bartleby.core
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]))

(defn map-values
  "Contruct a new map with all the values of the map kvs passed through f"
  [f kvs]
  (into {} (for [[k v] kvs] [k (f v)])))

(defn normalize-nfc
  "NFC-normalize the string s"
  [s]
  #?(:clj (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

(defn collapse-space
  "Replace all sequences of whitespace in s with a single space"
  [s]
  (str/replace s #"\s+" " "))

(defn wrap
  "Wrap the string s in left and right padding"
  ([s both] (wrap s both both))
  ([s left right] (str left s right)))

(defn xml-name
  "Sanitize the string s into a valid XML name.
  * Prefix with underscore if the first character is not a valid first character.
  * Remove any non- letter/number/some punctuation characters."
  [s]
  (-> s
      (str/replace #"^[^A-Za-z_:]" "_$0")
      (str/replace #"[^A-Za-z0-9._:-]" "")))

(defn tex->citekeys
  "Extract the citekeys from the TeX document string s. This uses simple
  regular expressions and will capture commented-out citations as well."
  [s]
  (->> (re-seq #"\\\w*cite\w*\{([^}]+)\}" s)
       ; re-seq returns a sequence of vectors, each N-groups long;
       ; we want the second group (the first captured group).
       (map second)
       ; split each csv multicite "adam:95,bron:1990" into its component parts
       (mapcat #(str/split % #","))
       (map str/trim)))

(defn aux->citekeys
  "Extract the citekeys from the TeX auxiliary-file string s (using regular expressions)"
  [s]
  ; BibLaTeX uses \abx@aux@cite{...}, BibTeX uses \bibcite{...}; we find either one:
  (for [[_ command citekey] (re-seq #"\\(abx@aux@cite|bibcite)\{([^}]+)\}" s)]
    citekey))

(defn bibtex?
  "Test that the input can be parsed as BibTeX"
  [input]
  (try
    ; parse the input character sequence non-lazily, to catch any potential errors
    (let [items (-> input bibtex/read-all doall)]
      ; it's not "BibTeX" if the only items are Gloss instances
      (some? (seq (remove :lines items))))
    (catch Exception e false)))

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
  (str/join " and "
    (if (> (count names) 2)
      ; group 1: take all but the last name, join those elements with a comma
      ; group 2: simply take the last name
      [(str/join ", " (butlast names)) (last names)]
      names)))

(defn reference->replacements
  "Generate sequence of replacement maps from the Reference r,
  with :match, :output, and :priority keys."
  [r]
  (let [{:keys [citekey fields]} r
        author (some->> fields (filter #(= (:key %) "author")) first :value author->lastnames format-names)
        year (->> fields (filter #(= (:key %) "year")) first :value)]
    (when (and author year)
      ; priority is so that greedier replacements happen first
      ; too ambiguous:
      ; {:match names
      ;  :output (format "\\citeauthor{%s}" citekey)
      ;  :priority 0}
      [{:match (format "%s %s" author year)
        :output (format "\\citealt{%s}" citekey)
        :priority 10}
       {:match (format "%s (%s)" author year)
        :output (format "\\citet{%s}" citekey)
        :priority 50}
       {:match (format "(%s %s)" author year)
        :output (format "\\citep{%s}" citekey)
        :priority 100}])))

(defn re-escape
  "Escape any regular expression syntax characters in s such that the result can be
  embedded in a regular expression, but will only match literally equivalent strings."
  [s]
  (str/replace s #"([.*+?^=!:${}()|[\\]/\\])" "\\\\$1"))

(defn interpolate
  "Replace literal names (plaintext citations) in the TeX document string tex with
  cite* commands, for names that are recognized by patterns generated from references."
  ; also need to support:
  ;   oxford commas
  ;   '&' as author separator instead of 'and'
  ;   et. al instead of all authors listed out
  [tex references]
  (let [replacements (sort-by :priority (mapcat reference->replacements references))
        matches (map :match replacements)
        all-matches-pattern (re-pattern (str/join \| (map re-escape matches)))]
    (str/replace tex all-matches-pattern
      (fn [group0]
        (get (->> replacements (filter #(= (:match %) group0)) first) :output (str "no output found for '" group0 "'"))))))
