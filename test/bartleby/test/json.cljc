(ns bartleby.test.json
  (:require [clojure.test :refer [deftest is testing]]
            [bartleby.test.bibliography :refer [tex->Field tex->Reference]]
            [bartleby.language.json :refer [toJSON fromJSON]]
            [bartleby.bibliography :refer [->Gloss]]))

(deftest test-toJSON
  (testing "JSONifying Gloss"
    (is (= {"lines" ["1" "2"]}
           (toJSON (->Gloss ["1" "2"])))))
  (testing "JSONifying Reference"
    (is (= {"pubtype" "book", "citekey" "benjamin", "title" "Reason"}
           (toJSON (tex->Reference "book" "benjamin" {"title" "Reason"}))))))

(deftest test-fromJSON
  (testing "parsing Gloss"
    (is (= (->Gloss ["1" "2"])
           (fromJSON {"lines" ["1" "2"]}))))
  (testing "parsing Reference"
    (is (= (tex->Reference "book" "benjamin" {"title" "Reason"})
           (fromJSON {"pubtype" "book", "citekey" "benjamin", "title" "Reason"})))))
