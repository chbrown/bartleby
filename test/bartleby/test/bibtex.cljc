(ns bartleby.test.bibtex
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [bartleby.language.tex :as tex]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.core :as core])
  (:import [bartleby.language.bibtex Field Reference Gloss]))

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
            actual (-> bibfile io/resource slurp bibtex/read-str .toJSON normalize-json)
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
    (let [actual (-> {"lines" ["1" "2"]} bibtex/fromJSON)
          expected (Gloss. ["1" "2"])]
      (is (= expected actual))))
  (testing "parsing Reference"
    (let [actual (-> {"pubtype" "book", "citekey" "benjamin", "title" "Reason"} bibtex/fromJSON)
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
  (testing "multi"
    (let [filename "examples/multi/paper.bib"
          input (-> filename io/resource io/reader core/char-seq)
          items (bibtex/read-all input)]
      (is (= 4 (count items)))
      (is (= "J93-2004" (-> items (nth 2) :citekey))))))
