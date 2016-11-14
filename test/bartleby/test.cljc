(ns bartleby.test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [bartleby.tex :as tex]
            [bartleby.bibtex :as bibtex]))

(defn normalize-nfc
  [s]
  #?(:clj (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

(defn normalize-value
  [value]
  (-> value
      (tex/tex-block->string)
      (normalize-nfc)
      (string/replace #"\s+" " ")))

(defn field->pair
  [{:keys [key value]}]
  [key (normalize-value value)])

(defn reference->object
  [{:keys [pubtype citekey fields]}]
  (into {"pubtype" pubtype
         "citekey" citekey}
    (map field->pair fields)))

(def bibfiles (->> (io/resource "resources")
                   (io/file)
                   (.list)
                   (filter #(string/ends-with? % ".bib"))))

(deftest test-bibtex-parser
  (doseq [bibfile bibfiles]
    (testing (str "should parse " bibfile " into json equivalent")
      (let [jsonfile (string/replace bibfile ".bib" ".json")
            ; is io/resource utf-8 by default?
            actual (-> bibfile io/resource slurp bibtex/parse first reference->object)
            expected (-> jsonfile io/resource slurp json/read-str)]
        ; clojure.test doesn't care about order but (= expected actual) is how humane-test-output reads it
        (is (= expected actual))))))
