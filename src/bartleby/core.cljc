(ns bartleby.core
  (:refer-clojure :exclude [read])
  (:require [clojure.string :as string]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]))

(defn map-values
  "contruct a new map with all the values of the given map passed through f"
  [f kvs]
  (into {} (for [[k v] kvs] [k (f v)])))

(defprotocol ReadableFile
  "general representation of a readable resource carrying a filename"
  (getName [this] "get the name of the file backing this resource")
  (read [this] "read a single character"))

; based on line-seq from standard library
(defn char-seq
  "Returns characters from rdr as a lazy sequence of strings."
  [rdr]
  ; .read: returns "The character read, as an integer [...],
  ; or -1 if the end of the stream has been reached"
  (lazy-seq
    (let [chr (.read rdr)]
      (when-not (neg? chr)
        (cons (char chr) (char-seq rdr))))))

(defn normalize-nfc
  "NFC-normalize the given string"
  [s]
  #?(:clj (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

(defn collapse-space
  [s]
  (string/replace s #"\s+" " "))

(defn tex->citekeys
  "Extract the citekeys in a TeX document (using regular expressions)"
  [s]
  ; super-simple regular expression solution (doesn't detect commented-out citations)
  ; re-seq returns a sequence of vectors, each N-groups long. We want the second group (the first captured group).
  ; the (apply concat ...) flattens the results
  ; TODO: find out if there's a better (for-cat [binding] ...) idiom in the std lib?
  (->> (for [[_ citekey-csv] (re-seq #"\\\w*cite\w*\{([^}]+)\}" s)]
         ; split each csv multicite \*cite*{albert:1995,brumhilda:1990,etc} into its component parts
         (string/split citekey-csv #","))
       (apply concat)
       (map string/trim)))

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
    ; parse the input char-seq, non-lazily, to catch any potential errors
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
                               (filter #(= (string/lower-case (:key %)) "crossref")) ; find crossref fields (case-insensitive)
                               (map :value) ; get field value
                               (set))
        all-citekeys (into citekeys crossref-citekeys)]
    ; if there are no new citekeys in all-citekeys (and thus, in crossref-citekeys), we're done
    (if (= all-citekeys citekeys)
      citekeys
      ; if there are new citekeys in crossref-citekeys, recurse.
      ; there should only be one level of crossrefs, maybe two; more than that is pathological
      (expand-citekeys items all-citekeys))))
