(ns bartleby.test.jats
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [bartleby.jats :as jats]
            [bartleby.bibliography :refer [->Field ->Reference ->Gloss]]))

(deftest test-write-str
  (testing "rendering Gloss"
    (let [gloss-xml (jats/write-str (->Gloss ["pagerange=1--9"]))]
      (is (str/includes? gloss-xml "<!-- pagerange=1–9 -->"))))
  (testing "rendering Reference"
    (let [fields (list (->Field "year" "1999")
                       (->Field "title" "Reason")
                       (->Field "author" "Whatler Benjamin")
                       (->Field "pages" "1--154")
                       (->Field "rating" "11/10"))
          reference-xml (jats/write-str (->Reference "book" "benj99" fields))]
      (is (str/includes? reference-xml "<ref id=\"benj99\">"))
      (is (str/includes? reference-xml "<year>1999</year>"))
      (is (str/includes? reference-xml "<article-title>Reason</article-title>"))
      (is (str/includes? reference-xml "<surname>Benjamin</surname>"))
      (is (str/includes? reference-xml "<given-names>Whatler</given-names>"))
      (is (str/includes? reference-xml "<lpage>154</lpage"))
      (is (str/includes? reference-xml "rating = 11/10")))))
