(ns bartleby.test.bibtex
  #?(:cljs (:require-macros [bartleby.test-resources :refer [read-resource-string read-resource-pairs]]))
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])
            [the.parsatron :refer [run]]
            #?(:clj [bartleby.test-resources :refer [read-resource-string read-resource-pairs]])
            [bartleby.test.bibliography :refer [tex->Field tex->Reference]]
            [bartleby.language.tex :as tex]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.json :refer [toJSON]]
            [bartleby.bibliography :refer [->Field ->Reference ->Gloss]]
            [bartleby.core :as core]
            [bartleby.util :as util]))

(defn- parse-json
  [json-string]
  #?(:clj  (json/read-str json-string)
     :cljs (js->clj (js/JSON.parse json-string))))

(defn- normalize-value
  [value]
  (some-> value util/normalize-unicode util/collapse-space))

(defn- normalize-json
  [obj]
  (util/map-values normalize-value obj))

(deftest test-bibtex-parser
  (let [pairs (read-resource-pairs)
        bibfiles (->> (keys pairs)
                      (filter #(str/ends-with? % ".bib"))
                      ; string interpolation is not yet supported
                      (remove #(str/includes? % "with-strings")))]
    (doseq [bibfile bibfiles
            :let [jsonfile (str/replace bibfile ".bib" ".json")]]
      (testing (str "parsing " bibfile " into json equivalent")
        (let [actual (-> bibfile pairs bibtex/read-str toJSON normalize-json)
              expected (-> jsonfile pairs parse-json)]
          ; clojure.test doesn't care about order but (= expected actual) is how humane-test-output reads it
          (is (= expected actual)))))))

(deftest test-write-str
  (testing "rendering Gloss"
    (let [actual (bibtex/write-str (->Gloss ["1" "2"]))
          expected "1\n2"]
      (is (= expected actual))))
  (testing "rendering Reference"
    (let [actual (bibtex/write-str (tex->Reference "book" "benjamin" {"title" "Reason"}))
          expected "@book{benjamin,\n  title = {Reason},\n}\n"]
      (is (= expected actual))))
  (testing "rendering Reference without citeky"
    (let [actual (bibtex/write-str (tex->Reference "book" nil {"title" "Apparent"}))
          expected "@book{\n  title = {Apparent},\n}\n"]
      (is (= expected actual)))))

(deftest test-examples
  (let [input (read-resource-string "examples/multi/paper.bib")]
    (testing "multi syntax"
      (is (core/bibtex? input)))
    (testing "multi"
      (let [items (bibtex/read-all input)]
        (is (= 4 (count items)))
        (is (= "J93-2004" (-> items (nth 2) :citekey)))))))

(deftest test-field-value-parser
  (testing "hard-casing block"
    (let [input "{{MALLET}: A Machine Learning for Language Toolkit}"]
      (is (= "{MALLET}: A Machine Learning for Language Toolkit"
             (tex/write-str (run (bibtex/field-value) input))))))
  (testing "command blocks"
    (let [input "{Putting the \\textbf{Par} back in \\emph{\\textbf{Par}}sing}"
          actual (tex/write-str (run (bibtex/field-value) input))
          expected "Putting the \\textbf{Par} back in \\emph{\\textbf{Par}}sing"]
      (is (= expected actual)))))

(deftest test-failure
  (testing "bad syntax"
    (is (not (core/bibtex? "@ :( sorry!")))))
