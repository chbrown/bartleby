(ns bartleby.test.json
  (:require [clojure.test :refer :all]
            [bartleby.language.json :refer [toJSON fromJSON]]
            [bartleby.bibliography :as bibliography :refer [->Field ->Reference ->Gloss]]))

(deftest test-toJSON
  (testing "JSONifying Gloss"
    (is (= {"lines" ["1" "2"]}
           (toJSON (->Gloss ["1" "2"])))))
  (testing "JSONifying Reference"
    (is (= {"pubtype" "book", "citekey" "benjamin", "title" "Reason"}
           (toJSON (->Reference "book" "benjamin" [(->Field "title" "Reason")]))))))

(deftest test-fromJSON
  (testing "parsing Gloss"
    (is (= (->Gloss ["1" "2"])
           (fromJSON {"lines" ["1" "2"]}))))
  (testing "parsing Reference"
    (is (= (->Reference "book" "benjamin" [(->Field "title" "Reason")])
           (fromJSON {"pubtype" "book", "citekey" "benjamin", "title" "Reason"})))))
