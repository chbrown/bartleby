(ns bartleby.test.cli
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [bartleby.cli :as cli]))

(deftest test-cat
  (let [command-fn (:cat cli/commands)
        inputs (-> "examples/multi/paper.bib" io/resource io/reader list)
        items (command-fn inputs {})]
    (is (= 4 (count items)))))

(deftest test-select
  (let [command-fn (:select cli/commands)
        filenames ["examples/multi/paper.bib" "examples/multi/paper.aux"]
        inputs (map #(-> % io/resource cli/file-reader) filenames)
        items (command-fn inputs {})]
    (is (= 2 (count items)))))

(deftest test-json
  (let [command-fn (:json cli/commands)
        inputs (-> "examples/multi/paper.bib" io/resource io/reader list)
        items (command-fn inputs {})]
    (is (= 4 (count items)))
    (is (every? #(string/starts-with? % "{") items))))

(deftest test-test
  (let [command-fn (:test cli/commands)
        filenames ["examples/multi/paper.bib" "examples/multi/paper.aux"]
        inputs (map #(-> % io/resource cli/file-reader) filenames)
        [bib-line aux-line] (command-fn inputs {})]
    (is (= "yes" bib-line))
    (is (= "no" aux-line))))
