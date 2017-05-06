(ns bartleby.test.tex
  (:require [clojure.test :refer :all]
            [bartleby.language.tex :as tex]
            [bartleby.core :as core]))

(def ^:private ->tex->str (comp core/normalize-nfc tex/-flatten tex/read-str))

(deftest test-read-str
  (testing "rendering accents"
    (is (= "ȧ é î õ ü" (->tex->str "\\.a \\'e \\^{i} \\~o \\\"u"))))
  (testing "rendering fancy characters"
    (is (= "ø ȷ" (->tex->str "\\o \\j"))))
  (testing "absorbing macros"
    (is (= "bold+italic" (->tex->str "\\emph{\\textbf{bold+italic}}"))))
  (testing "absorbing hyphenation hints"
    (is (= "luxuriously" (->tex->str "luxur\\-ious\\-ly"))))
  (testing "unescaping characters"
    (is (= "# ~" (->tex->str "\\# \\~"))))
  (testing "merging vacuous blocks"
    (is (= "ABC" (->tex->str "A{B{C}}")))))
