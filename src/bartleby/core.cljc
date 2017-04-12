(ns bartleby.core
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]))

(defn map-values
  "contruct a new map with all the values of the given map passed through f"
  [f kvs]
  (into {} (for [[k v] kvs] [k (f v)])))

(defn normalize-nfc
  "NFC-normalize the given string"
  [s]
  #?(:clj (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

(defn collapse-space
  "Replace all sequences of whitespace with a single space"
  [s]
  (str/replace s #"\s+" " "))

(defn wrap
  "Wrap the string s in left and right padding"
  ([s both] (wrap s both both))
  ([s left right] (str left s right)))

(defn xml-name
  "Sanitize the string s into a valid XML name.
  * prefix with underscore if the first character is not a valid first character
  * remove any non- letter/number/some punctuation characters"
  [s]
  (-> s
      (str/replace #"^[^A-Za-z_:]" "_$0")
      (str/replace #"[^A-Za-z0-9._:-]" "")))

(defn tex->citekeys
  "Extract the citekeys in a TeX document (using regular expressions)"
  [s]
  ; super-simple regular expression solution (doesn't detect commented-out citations)
  ; re-seq returns a sequence of vectors, each N-groups long. We want the second group (the first captured group).
  ; the (apply concat ...) flattens the results
  ; TODO: find out if there's a better (for-cat [binding] ...) idiom in the std lib?
  (->> (for [[_ citekey-csv] (re-seq #"\\\w*cite\w*\{([^}]+)\}" s)]
         ; split each csv multicite \*cite*{albert:1995,brumhilda:1990,etc} into its component parts
         (str/split citekey-csv #","))
       (apply concat)
       (map str/trim)))

(defn aux->citekeys
  "Extract the citekeys in an aux document (using regular expressions)"
  [s]
  ; BibLaTeX uses \abx@aux@cite{...}, BibTeX uses \bibcite{...}; we find either one:
  (for [[_ command citekey] (re-seq #"\\(abx@aux@cite|bibcite)\{([^}]+)\}" s)]
    citekey))

(defn bibtex?
  "Test that the given stream can be parsed as BibTeX"
  [input]
  (try
    ; parse the input character sequence non-lazily, to catch any potential errors
    (let [items (-> input bibtex/read-all doall)]
      ; it's not "BibTeX" if the only items are Gloss instances
      (some? (seq (remove :lines items))))
    (catch Exception e false)))

(defn expand-citekeys
  "recurse into crossref fields to find all used citekeys"
  [items citekeys]
  (let [citekeys (set citekeys)
        crossref-citekeys (->> items
                               (filter #(contains? citekeys (:citekey %))) ; find selected items
                               (mapcat :fields) ; flatten out to selected items' fields
                               (filter #(= (str/lower-case (:key %)) "crossref")) ; find crossref fields (case-insensitive)
                               (map :value) ; get field value
                               (set))
        all-citekeys (into citekeys crossref-citekeys)]
    ; if there are no new citekeys in all-citekeys (and thus, in crossref-citekeys), we're done
    (if (= all-citekeys citekeys)
      citekeys
      ; if there are new citekeys in crossref-citekeys, recurse.
      ; there should only be one level of crossrefs, maybe two; more than that is pathological
      (expand-citekeys items all-citekeys))))

(defn split-fullname
  "Parse fullname into a [given-names surname] (or just [given-names]) vector"
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

(defn reorder-name
  "standardize name parts from a single BibTeX chunk of a list of authors"
  [s]
  (let [parts (str/split s #"," 2) ; handle comma-separated names
        recombined (str/join \space (reverse parts))] ; no-op if there was no comma
    (str/split recombined #"\s+")))

(defn author->lastnames
  "get last names from BibTeX format"
  [author-value]
  (->> (str/split author-value #"\s+and\s+")
       (map reorder-name)
       (map last)))

(defn format-names
  "join a seq of (last-)names as it would normally be formatted"
  [names]
  ; [A] -> 'A'
  ; [A, B] -> 'A and B'
  ; [A, B, C] -> 'A, B and C' (no Oxford comma)
  (str/join " and "
    (if (> (count names) 2)
      ; group 1: take all but the last name, join those elements with a comma
      ; group 2: simply take the last name
      [(str/join ", " (butlast names)) (last names)]
      names)))

(defn reference->replacements
  [{:keys [citekey fields]}]
  (let [author (some->> fields (filter #(= (:key %) "author")) first :value author->lastnames format-names)
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

(defn re-escape [s] (str/replace s #"([.*+?^=!:${}()|[\\]/\\])" "\\\\$1"))

(defn interpolate
  "replace literal names (plaintext citations) with TeX cite commands for recognized names"
  ; also need to support:
  ;   oxford commas
  ;   '&' as author separator instead of 'and'
  ;   et. al instead of all authors listed out
  [tex-string references]
  (let [replacements (sort-by :priority (mapcat reference->replacements references))
        matches (map :match replacements)
        all-matches-pattern (re-pattern (str/join \| (map re-escape matches)))]
    (str/replace tex-string all-matches-pattern
      (fn [group0]
        (get (->> replacements (filter #(= (:match %) group0)) first) :output (str "no output found for '" group0 "'"))))))
