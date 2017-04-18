(ns bartleby.test.jats
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [bartleby.language.jats :as jats]
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
      (is (str/includes? reference-xml "rating = 11/10"))))
  (testing "rendering Reference with other fields"
    (let [fields (list (->Field "booktitle" "Collected Works")
                       (->Field "day" "31")
                       (->Field "doi" "10.1109/5.771073")
                       (->Field "edition" "10th")
                       (->Field "editor" "Katrina Pierce")
                       (->Field "institution" "Xerox PARC")
                       (->Field "isbn" "978-3-16-148410-0") ; "Courtly treasures"
                       (->Field "issn" "1021-9749") ; News from ISSN
                       (->Field "journal" "Language")
                       (->Field "month" "12")
                       (->Field "note" "Reprint")
                       (->Field "number" "5")
                       (->Field "publisher" "MIT Press")
                       (->Field "school" "CUNY")
                       (->Field "series" "Corrections")
                       (->Field "url" "https://en.wikipedia.org/wiki/BibTeX")
                       (->Field "volume" "4"))
          reference-xml (jats/write-str (->Reference "article" "reftest2" fields))]
      (is (str/includes? reference-xml "<source>Collected Works</source>"))
      (is (str/includes? reference-xml "<day>31</day>"))
      (is (str/includes? reference-xml "<pub-id pub-id-type=\"doi\">10.1109/5.771073</pub-id>"))
      (is (str/includes? reference-xml "<edition>10th</edition>"))
      (is (str/includes? reference-xml "<person-group person-group-type=\"editor\"><name><surname>Pierce</surname><given-names>Katrina</given-names></name></person-group>"))
      (is (str/includes? reference-xml "<institution>Xerox PARC</institution>"))
      (is (str/includes? reference-xml "<isbn>978-3-16-148410-0</isbn>"))
      (is (str/includes? reference-xml "<issn>1021-9749</issn"))
      (is (str/includes? reference-xml "<source>Language</source>"))
      (is (str/includes? reference-xml "<month>12</month>"))
      (is (str/includes? reference-xml "<comment>Reprint</comment>"))
      (is (str/includes? reference-xml "<issue>5</issue>"))
      (is (str/includes? reference-xml "<publisher-name>MIT Press</publisher-name>"))
      (is (str/includes? reference-xml "<institution>CUNY</institution>"))
      (is (str/includes? reference-xml "<series>Corrections</series>"))
      (is (str/includes? reference-xml "<uri>https://en.wikipedia.org/wiki/BibTeX</uri>"))
      (is (str/includes? reference-xml "<volume>4</volume>")))))
