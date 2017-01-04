(ns bartleby.test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [bartleby.language.tex :as tex]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.core :as core])
  (:import [bartleby.language.bibtex Reference Field]))

(defn- normalize-value
  [value]
  (some-> value tex/read core/normalize-nfc core/collapse-space))

(defn- normalize-json
  [obj]
  (core/map-values normalize-value obj))

(def bibfiles (->> (io/resource "resources")
                   (io/file)
                   (.list)
                   (filter #(string/ends-with? % ".bib"))
                   (remove #(string/includes? % "with-strings"))))

(deftest test-bibtex-parser
  (doseq [bibfile bibfiles]
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
