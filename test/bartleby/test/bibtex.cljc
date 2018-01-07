(ns bartleby.test.bibtex
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [the.parsatron :refer [run]]
            [bartleby.language.tex :as tex]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.json :refer [toJSON]]
            [bartleby.bibliography :as bibliography :refer [->Field ->Reference ->Gloss]]
            [bartleby.core :as core]
            [bartleby.util :as util]))

(defn- normalize-value
  [value]
  (some-> value util/normalize-nfc util/collapse-space))

(defn- normalize-json
  [obj]
  (util/map-values normalize-value obj))

(def pairs-bibfiles (->> (io/resource "resources/pairs")
                         (io/file)
                         (.list)
                         (filter #(str/ends-with? % ".bib"))
                         (remove #(str/includes? % "with-strings"))
                         (map #(str "pairs/" %))))

(deftest test-bibtex-parser
  (doseq [bibfile pairs-bibfiles]
    (testing (str "parsing " bibfile " into json equivalent")
      (let [jsonfile (str/replace bibfile ".bib" ".json")
            ; is io/resource utf-8 by default?
            actual (-> bibfile io/resource slurp bibtex/read-str toJSON normalize-json)
            expected (-> jsonfile io/resource slurp json/read-str)]
        ; clojure.test doesn't care about order but (= expected actual) is how humane-test-output reads it
        (is (= expected actual))))))

(deftest test-write-str
  (testing "rendering Gloss"
    (let [actual (bibtex/write-str (->Gloss ["1" "2"]))
          expected "1\n2"]
      (is (= expected actual))))
  (testing "rendering Reference"
    (let [actual (bibtex/write-str (->Reference "book" "benjamin" [(->Field "title" ["Reason"])]))
          expected "@book{benjamin,\n  title = {Reason},\n}\n"]
      (is (= expected actual))))
  (testing "rendering Reference without citeky"
    (let [actual (bibtex/write-str (->Reference "book" nil [(->Field "title" ["Apparent"])]))
          expected "@book{\n  title = {Apparent},\n}\n"]
      (is (= expected actual)))))

(deftest test-examples
  (let [filename "examples/multi/paper.bib"
        input (-> filename io/resource io/reader slurp)]
    (testing "multi syntax"
      (is (core/bibtex? input)))
    (testing "multi"
      (let [items (bibtex/read-all input)]
        (is (= 4 (count items)))
        (is (= "J93-2004" (-> items (nth 2) :citekey)))))))

(deftest test-field-value-parser
  (testing "hard-casing block"
    (let [input "{{MALLET}: A Machine Learning for Language Toolkit}"
          actual (tex/write-str (run (bibtex/field-value) input))
          expected "{MALLET}: A Machine Learning for Language Toolkit"]
      (is (= expected actual))))
  (testing "command blocks"
    (let [input "{Putting the \\textbf{Par} back in \\emph{\\textbf{Par}}sing}"
          actual (tex/write-str (run (bibtex/field-value) input))
          expected "Putting the \\textbf{Par} back in \\emph{\\textbf{Par}}sing"]
      (is (= expected actual)))))

(deftest test-failure
  (testing "bad syntax"
    (is (not (core/bibtex? "@ :( sorry!")))))
