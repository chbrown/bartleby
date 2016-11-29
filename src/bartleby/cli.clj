(ns bartleby.cli
  (:refer-clojure :exclude [line-seq])
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [bartleby.core :refer :all]
            [bartleby.language.bibtex :as bibtex]
            [clojure.java.io :as io])
  (:gen-class))

(defn filter-cited-from-filenames
  "Select only the cited entries from a bibliography, given a list of .tex and .bib files"
  [filenames]
  (let [tex-filenames (filter #(string/ends-with? % ".tex"))
        bib-filenames (filter #(string/ends-with? % ".bib"))]
    ; TODO: use -> ... (apply concat coll) instead of this weird twisty nested mess?
    (let [citekeys (mapcat #(tex->citekeys (io/reader %)) tex-filenames)
          references (mapcat #(bibtex/read-all (io/reader %)) bib-filenames)]
      (filter-cited references citekeys))))

(defn bib-format
  "Parse a stream of BibTeX to structured representation, and then format as standard BibTeX"
  [input output]
  ; (doseq ...) is non-lazy version of (for ...)
  (doseq [item (bibtex/read-all input)]
    (.write output (bibtex/write-str item))
    (.write output (str \newline))))

(defn bib-json
  "Parse a stream of BibTeX and format as JSON"
  [input output]
  (doseq [item (bibtex/read-all input)]
    (json/write (.toJSON item) output)
    (.write output (str \newline))))

(defn json-bib
  "Parse JSON-LD and format as standard BibTeX"
  [input output]
  (for [line (line-seq input)]
    (-> line
      (json/read-str)
      (bibtex/fromJSON)
      (bibtex/write output))))


(defmulti run (fn [input output [command & options]] command))
(defmethod run "cat"
  [input output args]
  (bib-format input output))
(defmethod run "json"
  [input output args]
  (bib-json input output))
(defmethod run "test"
  [input output args]
  ; TODO: take filenames, and print the name of each unparseable file to STDERR
  (.write output (if (bibtex? input) "yes\n" "no\n")))
(defmethod run :default
  [input output args]
  (.write output (apply str "unrecognized arguments: " args))
  (.write output "\n"))


(defn -main
  [& args]
  ;(println "Running bartleby.cli/main with args" args)
  ; io/reader doesn't cut it ("Don't know how to create ISeq from: java.io.BufferedReader")
  ; do stdin / stdout ever need (with-open [in/output *in/out*] ...) ?
  (run (char-seq *in*) *out* args))
