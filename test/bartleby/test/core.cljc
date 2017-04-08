(ns bartleby.test.core
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [bartleby.core :as core])
  (:import [bartleby.bibliography Field Reference]))

(deftest test-expand-citekeys
  (let [items [(Reference. "incollection" "zara" [(Field. "title" "Unrelated")])
               (Reference. "incollection" "adams" [(Field. "crossref" "benjamin")])
               (Reference. "book" "benjamin" [(Field. "title" "Related")])]
        expanded-citekeys (core/expand-citekeys items ["adams"])]
    (is (= #{"adams" "benjamin"} expanded-citekeys))))

(deftest test-citekeys
  (testing "extraction from tex"
    (let [actual (-> "examples/multi/paper.tex" io/resource slurp core/tex->citekeys)
          expected ["J93-2004" "papineni-EtAl:2002:ACL"]]
      (is (= expected actual))))
  (testing "extraction from aux"
    (let [actual (-> "examples/multi/paper.aux" io/resource slurp core/aux->citekeys)
          expected ["J93-2004" "papineni-EtAl:2002:ACL"]]
      (is (= expected actual)))))

(deftest test-split-fullname
  (is (= ["Andrea" "Littlemore"] (core/split-fullname "Andrea Littlemore")))
  (is (= ["Carrie E." "Donut"] (core/split-fullname "Carrie E. Donut")))
  (is (= ["Benjamin" "Adams"] (core/split-fullname "Adams, Benjamin")))
  (is (= ["Benjamin Haar" "Adams"] (core/split-fullname "Adams, Benjamin Haar")))
  (is (= ["Lucky" "von Duck"] (core/split-fullname "von Duck, Lucky")))
  ; TODO: add test for inferring patronyms from fullnames without commas
  (is (= ["Madonna"] (core/split-fullname "Madonna"))))

(deftest test-interpolate
  (let [references [(Reference. "book" "littlemore" [(Field. "author" "Andrea Littlemore")
                                                     (Field. "year" "2016")])
                    (Reference. "book" "adamsdonut" [(Field. "author" "Adams, Benjamin and Carrie Donut")
                                                     (Field. "year" "2007")])
                    (Reference. "book" "vital-etal" [(Field. "author" "Vital, Percy and Chambers, Vera and Lucky von Duck")
                                                     (Field. "year" "2010")])]
        tex-string "We saw most recently in Littlemore 2016 that the efforts of Vital, Chambers and Duck (2010) were not what they seemed (Adams and Donut 2007)."
        expected "We saw most recently in \\citealt{littlemore} that the efforts of \\citet{vital-etal} were not what they seemed \\citep{adamsdonut}."
        actual (core/interpolate tex-string references)]
    (is (= expected actual))))