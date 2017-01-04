(ns bartleby.test.tex
  (:require [clojure.test :refer :all]
            [bartleby.language.tex :as tex]
            [bartleby.core :as core]))

(deftest test-read-str
  (testing "rendering accents"
    (let [actual (-> "\\.a \\'e \\^{i} \\~o \\\"u" tex/read-str core/normalize-nfc)
          expected "ȧ é î õ ü"]
    (is (= expected actual))))
  (testing "rendering fancy characters"
    (let [actual (tex/read-str "\\o \\j")
          expected "ø ȷ"]
    (is (= expected actual))))
  (testing "absorbing macros"
    (let [actual (tex/read-str "\\emph{\\textbf{bold+italic}}")
          expected "bold+italic"]
    (is (= expected actual))))
  (testing "absorbing hyphenation hints"
    (let [actual (tex/read-str "luxur\\-ious\\-ly")
          expected "luxuriously"]
    (is (= expected actual))))
  (testing "unescaping characters"
    (let [actual (tex/read-str "\\# \\~")
          expected "# ~"]
    (is (= expected actual))))
  (testing "merging vacuous blocks"
    (let [actual (tex/read-str "A{B{C}}")
          expected "ABC"]
    (is (= expected actual)))))
