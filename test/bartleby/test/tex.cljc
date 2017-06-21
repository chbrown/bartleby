(ns bartleby.test.tex
  (:require [clojure.test :refer :all]
            [bartleby.language.tex :as tex]
            [bartleby.core :refer [tex->tex]]))

(deftest test-tex->tex
  (testing "rendering accents as combining characters"
    (is (= "ȧ é î õ ü" (tex->tex "\\.a \\'e \\^{i} \\~o \\\"u"))))

  (testing "rendering fancy characters"
    (is (= "øȷ" (tex->tex "\\o \\j"))))

  (testing "absorbing macros"
    (is (= "bold+italic" (tex->tex "\\emph{\\textbf{bold+italic}}"))))

  (testing "absorbing hyphenation hints"
    (is (= "luxuriously" (tex->tex "luxur\\-ious\\-ly"))))

  (testing "unescaping characters"
    (is (= "# ̃" (tex->tex "\\# \\~"))))

  (testing "merging vacuous blocks"
    (is (= "ABC" (tex->tex "A{B{C}}")))))
