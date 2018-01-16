(ns bartleby.test.tex
  (:require [clojure.test :refer [deftest is testing]]
            [bartleby.util :refer [normalize-unicode collapse-space]]
            [bartleby.language.tex :as tex]))

(defn tex->tex
  "Parse TeX string, interpret commands (flattening groups),
  write TeX string, and normalize to Unicode NFKC."
  [s]
  (-> s
      tex/read-str
      tex/interpret-commands
      (tex/write-str :flatten true)
      normalize-unicode
      collapse-space))

(deftest test-tex->tex
  (testing "rendering accents as combining characters"
    (is (= (normalize-unicode "ȧ é î õ ü")
           (tex->tex "\\.a \\'e \\^{i} \\~o \\\"u")))
    (is (= (normalize-unicode "ȧ é î õ ü")
           (tex->tex "\\. a \\' {e} \\^{ i} \\~{ o} \\\" { u}"))))

  (testing "rendering accents without anything to combine with"
    ; without the \relax, the last \= would be a syntax error
    ; Sublime Text 3 treats the combining caret weird; it should be U+0302 (COMBINING CIRCUMFLEX ACCENT),
    ; not the U+02C6 (MODIFIER LETTER CIRCUMFLEX ACCENT) that ST3 inserts.
    ; same with U+0060 (GRAVE ACCENT); it should be U+0300 (COMBINING GRAVE ACCENT)
    (is (= (normalize-unicode " ̇a  ́e  ̂y  ̃o  ̈ u  ̀  ̄")
           (tex->tex "\\.{{a}} \\' {{e}} \\^ { {y}} \\~ { {o}} \\\"{ { u}} \\`{} {\\=\\relax}"))))

  (testing "weird command placement"
    ; Command attachment is eager, left-to-right.
    ; So, \textbf\'a is treated like \textbf{\'}{a}, not \textbf{\'{a}}
    (is (= (normalize-unicode "´a")
           (tex->tex "\\textbf\\'a"))))

  (testing "rendering fancy characters"
    (is (= "øȷ" (tex->tex "\\o \\j")))
    (is (= "$ øk ȷ" (tex->tex "\\$ \\o k \\j")))
    (is (= "$økȷ" (tex->tex "\\$\\o{}k\\j"))))

  (testing "absorbing macros"
    (is (= "bold+italic" (tex->tex "\\emph{\\textbf{bold+italic}}"))))

  (testing "absorbing hyphenation hints"
    (is (= "luxuriously" (tex->tex "luxur\\-ious\\-ly"))))

  (testing "unescaping characters"
    ; technically, the \~ in this TeX has to be followed by a \relax or empty group or something
    (is (= (normalize-unicode "#  ̃")
           (tex->tex "\\# \\~\\relax")))
    (is (= (normalize-unicode "# ̃")
           (tex->tex "\\#\\~\\relax"))))

  (testing "merging vacuous blocks"
    (is (= "ABC" (tex->tex "A{B{C}}")))))
