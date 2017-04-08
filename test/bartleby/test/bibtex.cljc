(ns bartleby.test.bibtex
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [the.parsatron :refer [run]]
            [bartleby.language.tex :as tex]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.bibliography :as bibliography]
            [bartleby.core :as core])
  (:import [bartleby.bibliography Field Reference Gloss]))

(defn- normalize-value
  [value]
  (some-> value tex/read-str core/normalize-nfc core/collapse-space))

(defn- normalize-json
  [obj]
  (core/map-values normalize-value obj))

(def pairs-bibfiles (->> (io/resource "resources/pairs")
                         (io/file)
                         (.list)
                         (filter #(string/ends-with? % ".bib"))
                         (remove #(string/includes? % "with-strings"))
                         (map #(str "pairs/" %))))

(deftest test-bibtex-parser
  (doseq [bibfile pairs-bibfiles]
    (testing (str "parsing " bibfile " into json equivalent")
      (let [jsonfile (string/replace bibfile ".bib" ".json")
            ; is io/resource utf-8 by default?
            actual (-> bibfile io/resource slurp bibtex/read-str bibliography/toJSON normalize-json)
            expected (-> jsonfile io/resource slurp json/read-str)]
        ; clojure.test doesn't care about order but (= expected actual) is how humane-test-output reads it
        (is (= expected actual))))))

(deftest test-expand-citekeys
  (let [items [(Reference. "incollection" "zara" [(Field. "title" "Unrelated")])
               (Reference. "incollection" "adams" [(Field. "crossref" "benjamin")])
               (Reference. "book" "benjamin" [(Field. "title" "Related")])]
        expanded-citekeys (core/expand-citekeys items ["adams"])]
    (is (= #{"adams" "benjamin"} expanded-citekeys))))

(deftest test-fromJSON
  (testing "parsing Gloss"
    (let [actual (-> {"lines" ["1" "2"]} bibliography/fromJSON)
          expected (Gloss. ["1" "2"])]
      (is (= expected actual))))
  (testing "parsing Reference"
    (let [actual (-> {"pubtype" "book", "citekey" "benjamin", "title" "Reason"} bibliography/fromJSON)
          expected (Reference. "book" "benjamin" [(Field. "title" "Reason")])]
      (is (= expected actual)))))

(deftest test-write-str
  (testing "rendering Gloss"
    (let [actual (-> (Gloss. ["1" "2"]) bibtex/write-str)
          expected "1\n2"]
      (is (= expected actual))))
  (testing "rendering Reference"
    (let [actual (-> (Reference. "book" "benjamin" [(Field. "title" "Reason")]) bibtex/write-str)
          expected "@book{benjamin,\n  title = {Reason},\n}\n"]
      (is (= expected actual))))
  (testing "rendering Reference without citeky"
    (let [actual (-> (Reference. "book" nil [(Field. "title" "Apparent")]) bibtex/write-str)
          expected "@book{\n  title = {Apparent},\n}\n"]
      (is (= expected actual)))))

(deftest test-examples
  (let [filename "examples/multi/paper.bib"
        input (-> filename io/resource io/reader core/char-seq)]
    (testing "multi syntax"
      (is (core/bibtex? input)))
    (testing "multi"
      (let [items (bibtex/read-all input)]
        (is (= 4 (count items)))
        (is (= "J93-2004" (-> items (nth 2) :citekey)))))))

(deftest test-field-value-parser
  (testing "hard-casing block"
    (let [input "{{MALLET}: A Machine Learning for Language Toolkit}"
          actual (run (bibtex/field-value) input)
          expected "{MALLET}: A Machine Learning for Language Toolkit"]
      (is (= expected actual))))
  (testing "command blocks"
    (let [input "{Putting the \\textbf{Par} back in \\emph{\\textbf{Par}}sing}"
          actual (run (bibtex/field-value) input)
          expected "Putting the \\textbf{Par} back in \\emph{\\textbf{Par}}sing"]
      (is (= expected actual)))))

(deftest test-failure
  (testing "bad syntax"
    (is (not (core/bibtex? "@ :( sorry!")))))

(deftest test-citekeys
  (testing "extraction from tex"
    (let [actual (-> "examples/multi/paper.tex" io/resource slurp core/tex->citekeys)
          expected ["J93-2004" "papineni-EtAl:2002:ACL"]]
      (is (= expected actual))))
  (testing "extraction from aux"
    (let [actual (-> "examples/multi/paper.aux" io/resource slurp core/aux->citekeys)
          expected ["J93-2004" "papineni-EtAl:2002:ACL"]]
      (is (= expected actual)))))

(deftest test-interpolate
  (let [references [(Reference. "book" "littlemore" [(Field. "author" "Andrea Littlemore")
                                                     (Field. "year" "2016")])
                    (Reference. "book" "adamsdonut" [(Field. "author" "Adams, Benjamin and Carrie Donut")
                                                     (Field. "year" "2007")])
                    (Reference. "book" "vital-etal" [(Field. "author" "Vital, Percy and Chambers, Vera and Lucky von Duck")
                                                     (Field. "year" "2010")])]
        tex-string "We saw most recently in Littlemore 2016 that the efforts of Vital, Chambers and Duck (2010) were not what they seemed (Adams and Donut 2007)."
        expected "We saw most recently in \\citealt{littlemore} that the efforts of \\citet{vital-etal} were not what they seemed \\citep{adamsdonut}."
        actual (core/interpolate tex-string references)]
    (is (= expected actual))))
