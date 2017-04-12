(ns bartleby.test.bibliography
  (:require [clojure.test :refer :all]
            [bartleby.bibliography :as bibliography])
  (:import (bartleby.bibliography Field Reference Gloss)))

(deftest test-split-field
  (testing "splitting splittable field"
    (let [field (Field. "title" "All of Everything: Something Specific")
          subkey "subtitle"]
      (is (= [(Field. "title" "All of Everything")
              (Field. "subtitle" "Something Specific")] (bibliography/split-field field subkey)))))
  (testing "splitting unsplittable field"
    (let [field (Field. "title" "Just the Facts")
          subkey "subtitle"]
      (is (= [(Field. "title" "Just the Facts")] (bibliography/split-field field subkey))))))

(deftest test-fromJSON
  (testing "parsing Gloss"
    (let [actual (-> {"lines" ["1" "2"]} bibliography/fromJSON)
          expected (Gloss. ["1" "2"])]
      (is (= expected actual))))
  (testing "parsing Reference"
    (let [actual (-> {"pubtype" "book", "citekey" "benjamin", "title" "Reason"} bibliography/fromJSON)
          expected (Reference. "book" "benjamin" [(Field. "title" "Reason")])]
      (is (= expected actual)))))
