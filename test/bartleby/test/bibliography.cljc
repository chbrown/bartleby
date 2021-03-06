(ns bartleby.test.bibliography
  (:require [clojure.test :refer [deftest is testing]]
            [bartleby.language.tex :as tex]
            [bartleby.bibliography :as bibliography :refer [->Field ->Reference ->Gloss]]))

(defn tex->Field
  "Like ->Field, but `value-string` is parsed as TeX"
  [key value-string]
  {:pre [(string? key)
         (string? value-string)]}
  (->Field key (tex/read-str value-string)))

(defn tex->Reference
  "Like ->Reference, but `fields` is a map"
  [pubtype citekey fields]
  {:pre [(string? pubtype)
         (or (nil? citekey) (string? citekey))
         (map? fields)]}
  (->Reference pubtype citekey (map (fn [[k v]] (tex->Field k v)) fields)))

(deftest test-expand-citekeys
  (let [items [(tex->Reference "incollection" "zara" {"title" "Unrelated"})
               (tex->Reference "incollection" "adams" {"crossref" "benjamin"})
               (tex->Reference "book" "benjamin" {"title" "Related"})]]
    (is (= #{"adams" "benjamin"}
           (bibliography/expand-citekeys items ["adams"])))))

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
    (let [field (tex->Field "title" "All of Everything: Something Specific")
          subkey "subtitle"]
      (is (= (list (tex->Field "title" "All of Everything") (tex->Field "subtitle" "Something Specific"))
             (bibliography/split-field field subkey)))))
  (testing "splitting unsplittable field"
    (let [field (tex->Field "title" "Just the Facts")
          subkey "subtitle"]
      (is (= (list (tex->Field "title" "Just the Facts"))
             (bibliography/split-field field subkey))))))

(deftest test-fields-extract-subtitles
  (testing "extracting subtitles of splittable fields"
    (let [fields [(tex->Field "booktitle" "All of Everything: More or Less")]]
      (is (= [(tex->Field "booktitle" "All of Everything")
              (tex->Field "booksubtitle" "More or Less")] (#'bibliography/fields-extract-subtitles fields)))))
  (testing "extracting subtitles of already split field"
    (let [fields [(tex->Field "title" "All of Everything: More or Less")
                  (tex->Field "subtitle" "Something Specific")]]
      (is (= fields (#'bibliography/fields-extract-subtitles fields)))))
  (testing "extracting subtitles of non-title field"
    (let [fields [(tex->Field "journal" "Science: The Comment Sections")]]
      (is (= fields (#'bibliography/fields-extract-subtitles fields)))))
  (testing "extracting subtitles of already sub* field"
    (let [fields [(tex->Field "subtitle" "All or None: Some")]]
      (is (= fields (#'bibliography/fields-extract-subtitles fields))))))

(deftest test-extract-subtitles
  (testing "extracting subtitles of gloss"
    (let [gloss (->Gloss "Note to self: read this")]
      (is (= gloss (bibliography/extract-subtitles gloss)))))
  (testing "extracting subtitles of reference"
    (let [reference (tex->Reference "book" "benjamin" {"title" "A: B"})]
      (is (= (tex->Reference "book" "benjamin" {"title" "A" "subtitle" "B"})
             (bibliography/extract-subtitles reference))))))

(deftest test-fields-embed-subtitles
  (testing "embedding subtitles of split fields"
    (let [fields [(tex->Field "title" "All of Everything")
                  (tex->Field "subtitle" "More or Less")]]
      (is (= [(tex->Field "title" "All of Everything: More or Less")]
             (#'bibliography/fields-embed-subtitles fields)))))
  (testing "embedding subtitles of unsplit field"
    (let [fields [(tex->Field "title" "All of Everything: More or Less")]]
      (is (= fields (#'bibliography/fields-embed-subtitles fields)))))
  (testing "embedding subtitles of sole subtitle field (edge-case)"
    (let [fields [(tex->Field "subtitle" "More or Less")]]
      (is (= [(tex->Field "title" "More or Less")] (#'bibliography/fields-embed-subtitles fields))))))

(deftest test-embed-subtitles
  (testing "embedding subtitles of gloss (no-op)"
    (let [gloss (->Gloss "Note to self")]
      (is (= gloss (bibliography/embed-subtitles gloss)))))
  (testing "embedding subtitles of reference"
    (let [reference (tex->Reference "book" "benjamin" {"title" "A"
                                                       "subtitle" "B"})]
      (is (= (tex->Reference "book" "benjamin" {"title" "A: B"})
             (bibliography/embed-subtitles reference))))))
