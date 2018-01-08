(ns bartleby.core
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as str]
            [bartleby.util :refer [re-escape author->lastnames format-names]]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]))

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
  (for [[_ _ citekey] (re-seq #"\\(abx@aux@cite|bibcite)\{([^}]+)\}" s)]
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

(defn reference->replacements
  "Generate sequence of replacement maps from the Reference r,
  with :match, :output, and :priority keys."
  [r]
  (let [{:keys [citekey fields]} r
        author (some->> fields (filter #(= (:key %) "author")) first :value tex/simplify tex/write-str author->lastnames format-names)
        year (->> fields (filter #(= (:key %) "year")) first :value tex/simplify tex/write-str)]
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

(defn interpolate
  "Replace literal names (plaintext citations) in the TeX document string tex with
  cite* commands, for names that are recognized by patterns generated from references."
  ; also need to support:
  ;   oxford commas
  ;   '&' as author separator instead of 'and'
  ;   et. al instead of all authors listed out
  [tex references]
  (let [replacements (sort-by :priority (mapcat reference->replacements references))
        ; prepare mapping from match string to output string
        replacements-lookup (reduce #(assoc %1 (:match %2) (:output %2)) {} replacements)
        all-matches-pattern (->> (map :match replacements)
                                 (map re-escape)
                                 (str/join \|)
                                 (re-pattern))]
    (str/replace tex all-matches-pattern #(get replacements-lookup % (str "no output found for '" % "'")))))
