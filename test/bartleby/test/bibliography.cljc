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
    (is (= (Gloss. ["1" "2"])
           (bibliography/fromJSON {"lines" ["1" "2"]}))))
  (testing "parsing Reference"
    (is (= (Reference. "book" "benjamin" [(Field. "title" "Reason")])
           (bibliography/fromJSON {"pubtype" "book", "citekey" "benjamin", "title" "Reason"})))))

(deftest test-toJSON
  (testing "JSONifying Gloss"
    (is (= {"lines" ["1" "2"]}
           (bibliography/toJSON (Gloss. ["1" "2"])))))
  (testing "JSONifying Reference"
    (is (= {"pubtype" "book", "citekey" "benjamin", "title" "Reason"}
           (bibliography/toJSON (Reference. "book" "benjamin" [(Field. "title" "Reason")]))))))
