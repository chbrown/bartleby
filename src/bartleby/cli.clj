(ns bartleby.cli
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]
            [clojure.java.io :as io])
  (:gen-class))

(defn- filename->citekeys
  [filename]
  (condp #(string/ends-with? %2 %1) filename
    ".tex" (-> filename slurp core/tex->citekeys)
    ".aux" (-> filename slurp core/aux->citekeys)
    []))

(defn- select-cited-from-filenames
  "Select only the cited entries from a bibliography, given a list of .tex and .bib files"
  [filenames]
  (let [citekeys (mapcat filename->citekeys filenames)
        citekeys-set (set citekeys)
        ; only keep items that are references that have been cited, or are not references
        keep? (fn [item] (or (nil? (:citekey item)) (contains? citekeys-set (:citekey item))))
        bib-filenames (filter #(string/ends-with? % ".bib") filenames)
        items (mapcat #(-> % io/reader core/char-seq bibtex/read-all) bib-filenames)]
    (filter keep? items)))

(defn- write-item
  [output item]
  (.write output (str (bibtex/write-str item :trailing-comma? true, :=-padded? true) \newline)))

; call run like: (run input output [command options...])
(defmulti run (fn [_ _ [command & _]] command))
(defmethod run "cat"
  ; Parse a stream of BibTeX to structured representation, and then format as standard BibTeX
  [input output args]
  (doseq [item (bibtex/read-all input)]
    (write-item output item)))
(defmethod run "json"
  ; Parse a stream of BibTeX and format as JSON
  [input output args]
  (doseq [item (bibtex/read-all input)]
    (.write output (str (json/write-str (.toJSON item)) \newline))))
(defmethod run "json2bib"
  ; Parse JSON-LD and format as standard BibTeX
  [input output args]
  (doseq [line (line-seq input)]
    (->> line
         (json/read-str)
         (bibtex/fromJSON)
         (write-item output))))
(defmethod run "test"
  [input output args]
  ; TODO: take filenames, and print the name of each unparseable file to STDERR
  (.write output (if (core/bibtex? input) "yes\n" "no\n")))
(defmethod run "select"
  [_ output args]
  (doseq [item (select-cited-from-filenames args)]
    (write-item output item)))
(defmethod run :default
  [input output args]
  (.write output (str "unrecognized arguments: " (apply str args) \newline)))

(defn -main
  [& args]
  ; *in* returns (LineNumberingPushbackReader. System/in), which is an
  ; io/reader, but io/reader doesn't cut it:
  ; "Don't know how to create ISeq from: java.io.BufferedReader"
  (run (core/char-seq *in*) *out* args)
  ; weirdly, System/out doesn't always get automatically flushed
  (.flush *out*))
