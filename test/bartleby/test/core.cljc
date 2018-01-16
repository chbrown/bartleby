(ns bartleby.test.core
  #?(:cljs (:require-macros [bartleby.test-resources :refer [read-resource-string]]))
  (:require [clojure.test :refer [deftest is testing]]
            #?(:clj [bartleby.test-resources :refer [read-resource-string]])
            [bartleby.test.bibliography :refer [tex->Reference]]
            [bartleby.language.tex :as tex]
            [bartleby.core :as core]
            [bartleby.util :as util]))

(deftest test-citekeys
  (testing "extraction from tex"
    (let [actual (core/tex->citekeys (read-resource-string "examples/multi/paper.tex"))
          expected ["J93-2004" "papineni-EtAl:2002:ACL"]]
      (is (= expected actual))))
  (testing "extraction from aux"
    (let [actual (core/aux->citekeys (read-resource-string "examples/multi/paper.aux"))
          expected ["J93-2004" "papineni-EtAl:2002:ACL"]]
      (is (= expected actual)))))

(deftest test-split-fullname
  (is (= ["Andrea" "Littlemore"] (util/split-fullname "Andrea Littlemore")))
  (is (= ["Carrie E." "Donut"] (util/split-fullname "Carrie E. Donut")))
  (is (= ["Benjamin" "Adams"] (util/split-fullname "Adams, Benjamin")))
  (is (= ["Benjamin Haar" "Adams"] (util/split-fullname "Adams, Benjamin Haar")))
  (is (= ["Lucky" "von Duck"] (util/split-fullname "von Duck, Lucky")))
  ; TODO: add test for inferring patronyms from fullnames without commas
  (is (= ["Madonna"] (util/split-fullname "Madonna"))))

(deftest test-interpolate
  (let [references [(tex->Reference "book" "littlemore" {"author" "Andrea Littlemore"
                                                         "year" "2016"})
                    (tex->Reference "book" "adamsdonut" {"author" "Adams, Benjamin and Carrie Donut"
                                                         "year" "2007"})
                    (tex->Reference "book" "vital-etal" {"author" "Vital, Percy and Chambers, Vera and Lucky von Duck"
                                                         "year" "2010"})]
        raw-document (tex/read-str "C.f. Littlemore 2016 wherein Vital, Chambers and Duck (2010) disagreed (Adams and Donut 2007).")]
    (is (= (tex/read-str "C.f. \\citealt{littlemore} wherein \\citet{vital-etal} disagreed \\citep{adamsdonut}.")
           (core/interpolate raw-document references)))))
