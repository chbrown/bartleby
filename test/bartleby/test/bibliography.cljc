(ns bartleby.test.bibliography
  (:require [clojure.test :refer [deftest is testing]]
            [bartleby.bibliography :as bibliography :refer [->Field ->Reference ->Gloss]]))

(deftest test-expand-citekeys
  (let [items [(->Reference "incollection" "zara" [(->Field "title" "Unrelated")])
               (->Reference "incollection" "adams" [(->Field "crossref" "benjamin")])
               (->Reference "book" "benjamin" [(->Field "title" "Related")])]
        expanded-citekeys (bibliography/expand-citekeys items ["adams"])]
    (is (= #{"adams" "benjamin"} expanded-citekeys))))

(deftest test-title-case
  (is (= "Lowercase" (bibliography/title-case "lowercase")))
  (is (= "Uppercase" (bibliography/title-case "UPPERCASE")))
  (is (= "Mixedcase" (bibliography/title-case "MiXedcAsE")))
  (is (= "Lower Case" (bibliography/title-case "lower case")))
  (is (= "Upper Case" (bibliography/title-case "UPPER CASE")))
  (is (= "Mixed Case" (bibliography/title-case "MiXed cAsE"))))

(deftest test-match-case
  (is (= "sample" (bibliography/match-case "SAMPLE" "lowercase")))
  (is (= "SAMPLE" (bibliography/match-case "sample" "UPPERCASE")))
  (is (= "Sample text" (bibliography/match-case "Sample Text" "Capitalized text")))
  (is (= "Sample Text" (bibliography/match-case "sample text" "Title Case")))
  (is (= "SaMpLe" (bibliography/match-case "SaMpLe" "MiXeD"))))

(deftest test-split-field
  (testing "splitting splittable field"
    (let [field (->Field "title" "All of Everything: Something Specific")
          subkey "subtitle"]
      (is (= (list (->Field "title" "All of Everything") (->Field "subtitle" "Something Specific"))
             (bibliography/split-field field subkey)))))
  (testing "splitting unsplittable field"
    (let [field (->Field "title" "Just the Facts")
          subkey "subtitle"]
      (is (= (list (->Field "title" "Just the Facts"))
             (bibliography/split-field field subkey))))))

(deftest test-fields-extract-subtitles
  (testing "extracting subtitles of splittable fields"
    (let [fields [(->Field "booktitle" "All of Everything: More or Less")]]
      (is (= [(->Field "booktitle" "All of Everything")
              (->Field "booksubtitle" "More or Less")] (#'bibliography/fields-extract-subtitles fields)))))
  (testing "extracting subtitles of already split field"
    (let [fields [(->Field "title" "All of Everything: More or Less")
                  (->Field "subtitle" "Something Specific")]]
      (is (= fields (#'bibliography/fields-extract-subtitles fields)))))
  (testing "extracting subtitles of non-title field"
    (let [fields [(->Field "journal" "Science: The Comment Sections")]]
      (is (= fields (#'bibliography/fields-extract-subtitles fields)))))
  (testing "extracting subtitles of already sub* field"
    (let [fields [(->Field "subtitle" "All or None: Some")]]
      (is (= fields (#'bibliography/fields-extract-subtitles fields))))))

(deftest test-extract-subtitles
  (testing "extracting subtitles of gloss"
    (let [gloss (->Gloss "Note to self: read this")]
      (is (= gloss (bibliography/extract-subtitles gloss)))))
  (testing "extracting subtitles of reference"
    (let [reference (->Reference "book" "benjamin" [(->Field "title" "A: B")])]
      (is (= (->Reference "book" "benjamin" [(->Field "title" "A") (->Field "subtitle" "B")])
             (bibliography/extract-subtitles reference))))))

(deftest test-fields-embed-subtitles
  (testing "embedding subtitles of split fields"
    (let [fields [(->Field "title" "All of Everything")
                  (->Field "subtitle" "More or Less")]]
      (is (= [(->Field "title" (seq "All of Everything: More or Less"))]
             (#'bibliography/fields-embed-subtitles fields)))))
  (testing "embedding subtitles of unsplit field"
    (let [fields [(->Field "title" "All of Everything: More or Less")]]
      (is (= fields (#'bibliography/fields-embed-subtitles fields)))))
  (testing "embedding subtitles of sole subtitle field (edge-case)"
    (let [fields [(->Field "subtitle" "More or Less")]]
      (is (= [(->Field "title" "More or Less")] (#'bibliography/fields-embed-subtitles fields))))))

(deftest test-embed-subtitles
  (testing "embedding subtitles of gloss (no-op)"
    (let [gloss (->Gloss "Note to self")]
      (is (= gloss (bibliography/embed-subtitles gloss)))))
  (testing "embedding subtitles of reference"
    (let [reference (->Reference "book" "benjamin" [(->Field "title" "A")
                                                    (->Field "subtitle" "B")])]
      (is (= (->Reference "book" "benjamin" [(->Field "title" (seq "A: B"))])
             (bibliography/embed-subtitles reference))))))
