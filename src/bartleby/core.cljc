(ns bartleby.core
  (:refer-clojure :exclude [line-seq])
  (:require [clojure.string :as string]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]))

(defn update-when
  "Like update but only calls f if m contains k"
  [m k f & args]
  (if (contains? m k) (apply update m k f args) m))

(defn line-seq
  "straight from the standard library but without the BufferedReader type assertion"
  [rdr]
  ; .readLine returns "A String containing the contents of the line,
  ; not including any line-termination characters,
  ; or null if the end of the stream has been reached"
  (when-let [line (.readLine rdr)]
    (cons line (lazy-seq (line-seq rdr)))))

; based on line-seq from standard library
(defn char-seq
  "Returns characters from rdr as a lazy sequence of strings."
  [rdr]
  ; .read: returns "The character read, as an integer [...],
  ; or -1 if the end of the stream has been reached"
  (let [chr (.read rdr)]
    (when-not (neg? chr)
      (cons (char chr) (lazy-seq (char-seq rdr))))))

(defn normalize-nfc
  "NFC-normalize the given string"
  [s]
  #?(:clj (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

(defn collapse-space
  [s]
  (string/replace s #"\s+" " "))

(defn normalize-bibtex-item
  [item]
  ; update :fields if it exists
  (update-when item :fields
    (fn [fields]
      (for [{:keys [key value]} fields]
        [key (-> value tex/read normalize-nfc collapse-space)]))))

(defn tex->citekeys
  "Extract the citekeys references in a TeX document (using RegExp)"
  [tex-string]
  (->> tex-string
    ; super-simple regular expression solution (doesn't detect commented-out citations)
    (re-seq #"\\\w*cite\w*\{([^}]+)\}")
    ; split each csv multicite \*cite*{albert:1995,brumhilda:1990,etc} into its component parts and flatten
    (mapcat #(string/split % #","))))

(defn filter-cited
  "Select only the cited entries from a bibliography, given a list of .tex and .bib files"
  [references citekeys]
  (filter (fn [reference] (contains? citekeys (:citekey reference))) references))

(defn bibtex?
  "Test that the given stream can be parsed as BibTeX"
  [input]
  (try
    (dorun (bibtex/read-all input))
    true
    (catch Exception e false)))
