(ns bartleby.test.tex
  (:require [clojure.test :refer [deftest is testing]]
            [bartleby.core :refer [tex->tex]]))

(deftest test-tex->tex
  (testing "rendering accents as combining characters"
    (is (= "ȧ é î õ ü" (tex->tex "\\.a \\'e \\^{i} \\~o \\\"u")))
    (is (= "ȧ é î õ ü" (tex->tex "\\. a \\' {e} \\^{ i} \\~{ o} \\\" { u}"))))

  (testing "rendering accents without anything to combine with"
    (is (= " ̇a  ́e ˆy  ̃o  ̈ u `  ̄" (tex->tex "\\.{{a}} \\' {{e}} \\^ { {y}} \\~ { {o}} \\\"{ { u}} \\`{} {\\=}"))))

  (testing "weird command placement"
    ; Command attachment is eager, left-to-right.
    ; So, \textbf\'a is treated like \textbf{\'}{a}, not \textbf{\'{a}}
    (is (= "´a" (tex->tex "\\textbf\\'a"))))

  (testing "rendering fancy characters"
    (is (= "øȷ" (tex->tex "\\o \\j")))
    (is (= "$ øk ȷ" (tex->tex "\\$ \\o k \\j")))
    (is (= "$økȷ" (tex->tex "\\$\\o{}k\\j"))))

  (testing "absorbing macros"
    (is (= "bold+italic" (tex->tex "\\emph{\\textbf{bold+italic}}"))))

  (testing "absorbing hyphenation hints"
    (is (= "luxuriously" (tex->tex "luxur\\-ious\\-ly"))))

  (testing "unescaping characters"
    ; technically, this TeX would need to be followed by a \relax or empty group or something
    (is (= "# ̃" (tex->tex "\\# \\~"))))

  (testing "merging vacuous blocks"
    (is (= "ABC" (tex->tex "A{B{C}}")))))
