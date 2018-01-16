(ns bartleby.core
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
    (catch #?(:clj Exception :cljs js/Error) e false)))

(defn reference->replacements
  "Generate sequence of replacement maps from the Reference r,
  with :match, :output, and :priority keys."
  [r]
  (let [{:keys [citekey fields]} r
        author (some->> fields (filter #(= (:key %) "author")) first :value tex/interpret-commands tex/write-str author->lastnames format-names)
        year (->> fields (filter #(= (:key %) "year")) first :value tex/interpret-commands tex/write-str)]
    (when (and author year)
      ; priority is so that greedier replacements happen first
      ; too ambiguous:
      ; {:match names
      ;  :output (format "\\citeauthor{%s}" citekey)
      ;  :priority 0}
      [{:match (str author " " year)
        :output (str "\\citealt{" citekey "}")
        :priority 10}
       {:match (str author " (" year ")")
        :output (str "\\citet{" citekey "}")
        :priority 50}
       {:match (str "(" author " " year ")")
        :output (str "\\citep{" citekey "}")
        :priority 100}])))

(defn interpolate
  "Replace literal names (plaintext citations) in the TeX Group `document`
  with cite* commands, for names that are recognized by patterns generated
  from `references`."
  ; also need to support:
  ;   oxford commas
  ;   '&' as author separator instead of 'and'
  ;   et. al instead of all authors listed out
  [document references]
  {:pre  [(tex/Group? document)]
   :post [(tex/Group? %)]}
  (let [replacements (sort-by (comp - :priority) (mapcat reference->replacements references))
        ; prepare mapping from match string to output string
        replacements-lookup (into {} (map (fn [{:keys [match output]}]
                                            [(:tokens (tex/read-str match)) (:tokens (tex/read-str output))]) replacements))
        replacer (fn [m] (get replacements-lookup (tex/write-str m)
                              (str "no output found for '" (tex/write-str m) "'")))]
    (reduce (fn [document pattern]
              (tex/replace document (:tokens (tex/read-str pattern)) replacer)) document (map :match replacements))))
