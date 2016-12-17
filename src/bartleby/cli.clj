(ns bartleby.cli
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]
            [clojure.java.io :as io])
  (:gen-class))

(defn filename->citekeys
  [filename]
  (condp #(string/ends-with? %2 %1) filename
    ".tex" (-> filename slurp core/tex->citekeys)
    ".aux" (-> filename slurp core/aux->citekeys)
    []))

(defn select-cited-from-filenames
  "Select only the cited entries from a bibliography, given a list of .tex and .bib files"
  [filenames]
  (let [citekeys (mapcat filename->citekeys filenames)
        citekeys-set (set citekeys)
        ; only keep items that are references that have been cited, or are not references
        keep? (fn [item] (or (nil? (:citekey item)) (contains? citekeys-set (:citekey item))))
        bib-filenames (filter #(string/ends-with? % ".bib") filenames)
        items (mapcat #(-> % io/reader core/char-seq bibtex/read-all) bib-filenames)]
    (filter keep? items)))

; call run like: (run input output [command options...])
(defmulti run (fn [_ _ [command & _]] command))
(defmethod run "cat"
  [input output args]
  ; (doseq ...) is non-lazy version of (for ...)
  ; Parse a stream of BibTeX to structured representation, and then format as standard BibTeX
  (doseq [item (bibtex/read-all input)]
    (.write output (bibtex/write-str item {:trailing-comma? false, :=-padded? false}))
    (.write output (str \newline))))
(defmethod run "json"
  ; Parse a stream of BibTeX and format as JSON
  [input output args]
  (doseq [item (bibtex/read-all input)]
    (json/write (.toJSON item) output)
    (.write output (str \newline))))
;(defn json-bib
;  "Parse JSON-LD and format as standard BibTeX"
;  [input output]
;  (for [line (line-seq input)]
;    (-> line
;        (json/read-str)
;        (bibtex/fromJSON)
;        (bibtex/write output))))
(defmethod run "test"
  [input output args]
  ; TODO: take filenames, and print the name of each unparseable file to STDERR
  (.write output (if (core/bibtex? input) "yes\n" "no\n")))
(defmethod run "select"
  [_ output args]
  (let [selected-references (select-cited-from-filenames args)]
    (doseq [item selected-references]
      (.write output (str (bibtex/write-str item {:trailing-comma? false, :=-padded? false}) \newline)))))
(defmethod run :default
  [input output args]
  (.write output (apply str "unrecognized arguments: " args))
  (.write output "\n"))

(defn -main
  [& args]
  ; *in* returns (LineNumberingPushbackReader. System/in)
  ; (io/reader *in*) -- convert to java.io.Reader, specifically, a java.io.BufferedReader
  ;(println "Running bartleby.cli/main with args" args)
  ; io/reader doesn't cut it ("Don't know how to create ISeq from: java.io.BufferedReader")
  ; do stdin / stdout ever need (with-open [in/output *in/out*] ...) ?
  (run (core/char-seq *in*) *out* args))
