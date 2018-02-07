(ns bartleby.test.util
  (:require [clojure.test :refer [deftest is are testing]]
            [bartleby.util :as util]))

(deftest test-blank?
  (testing "blanks"
    (are [x] (util/blank? x)
         nil
         \space
         \tab
         ""
         "\n\n   "))
  (testing "non-blanks"
    (are [x] (not (util/blank? x))
         \a
         false
         "Hello"
         #{}
         "\n\nEND")))

(deftest test-collapse-space
  (are [expected s] (= expected (util/collapse-space s))
       "A B C" "A\nB\n C"
       "A Z "  "A \tZ\n\n\n"))

(deftest test-map-values
  (is (= {:a 2 :b 3} (util/map-values inc {:a 1 :b 2})))
  (is (= {} (util/map-values inc {}))))

(deftest test-split-fullname
  (are [expected fullname] (= expected (util/split-fullname fullname))
       ["Ada"]                    "Ada"
       ["Ada" "Lovelace"]         "Lovelace, Ada"
       ["Ada" "Lovelace"]         "Ada Lovelace"
       ["Augusta Ada" "Lovelace"] "Lovelace, Augusta Ada"
       ["Augusta Ada" "Lovelace"] "Augusta Ada Lovelace"))

(deftest test-author->lastnames
  (are [expected fullnames] (= expected (util/author->lastnames fullnames))
       ["Lands" "Kerr"] "Annie Lands and Kelly Kerr"
       ["Linn"]         "Ari H. Linn"))

(deftest test-format-names
  (are [expected names] (= expected (util/format-names names))
       "A"          ["A"]
       "A and B"    ["A" "B"]
       "A, B and C" ["A" "B" "C"]))
