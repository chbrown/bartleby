(ns bartleby.test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.core :as core]))

(def bibfiles (->> (io/resource "resources")
                   (io/file)
                   (.list)
                   (filter #(string/ends-with? % ".bib"))
                   (remove #(string/includes? % "with-strings"))))

(deftest test-bibtex-parser
  (doseq [bibfile bibfiles]
    (testing (str "parsing " bibfile)
      (let [jsonfile (string/replace bibfile ".bib" ".json")
            ; is io/resource utf-8 by default?
            actual (-> bibfile io/resource slurp bibtex/read-str core/normalize-bibtex-item .toJSON)
            expected (-> jsonfile io/resource slurp json/read-str)]
        ; clojure.test doesn't care about order but (= expected actual) is how humane-test-output reads it
        (is (= expected actual))))))
