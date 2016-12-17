(ns bartleby.core
  (:require [clojure.string :as string]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]))

(defn update-when
  "Like update but only calls f if m contains k"
  [m k f & args]
  (if (contains? m k) (apply update m k f args) m))

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

(defn normalize-bibtex-item
  [item]
  ; update :fields if it exists
  (update-when item :fields
    (fn [fields]
      (for [{:keys [key value]} fields]
        [key (-> value tex/read normalize-nfc collapse-space)]))))

(defn tex->citekeys
  "Extract the citekeys in a TeX document (using regular expressions)"
  [s]
  ; super-simple regular expression solution (doesn't detect commented-out citations)
  ; re-seq returns a sequence of vectors, each N-groups long. We want the second group (the first captured group).
  ; the (apply concat ...) flattens the results
  ; TODO: find out if there's a better (for-cat [binding] ...) idiom in the std lib?
  (apply concat (for [[_ citekey-csv] (re-seq #"\\\w*cite\w*\{([^}]+)\}" s)]
                  ; split each csv multicite \*cite*{albert:1995,brumhilda:1990,etc} into its component parts
                  (string/split citekey-csv #","))))

(defn aux->citekeys
  "Extract the citekeys in an aux document (using regular expressions)"
  [s]
  (map second (re-seq #"\\abx@aux@cite\{([^}]+)\}" s)))

(defn bibtex?
  "Test that the given stream can be parsed as BibTeX"
  [input]
  (try
    (dorun (bibtex/read-all input))
    true
    (catch Exception e false)))
