(ns bartleby.test.cli
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [bartleby.language.tex :as tex]
            [bartleby.cli :as cli]))

(defn- resource->named-reader
  [name]
  (cli/->NamedReader name (io/reader (io/resource name))))

(deftest test-main-exits
  (with-redefs [cli/exit! identity]
    (testing "help"
      (let [output (with-out-str (cli/-main "--help"))]
        (is (str/includes? output "Usage: bart"))))
    (testing "version"
      (let [output (with-out-str (cli/-main "--version"))]
        (is (str/starts-with? output "bartleby "))
        (is (not (str/includes? output "Usage")))))
    (testing "no args"
      (let [output (with-out-str (cli/-main))]
        (is (str/includes? output "Usage: bart"))
        (is (str/includes? output "must supply a command"))))
    (testing "bad command"
      (let [output (with-out-str (cli/-main "magic-fix"))]
        (is (str/includes? output "Usage: bart"))
        (is (str/includes? output "unrecognized command 'magic-fix'"))))
    (testing "tools.cli parse-opts error"
      (let [output (with-out-str (cli/-main "cat" "--magic-fix"))]
        (is (str/includes? output "Unknown option: \"--magic-fix\""))))))

(deftest test-main-cat
  (testing "from filepath"
    (let [output (with-out-str (cli/-main "cat" "test/resources/examples/multi/paper.bib"))]
      (is (str/includes? output "@article{Lowry01111951"))
      (is (str/includes? output "@InProceedings{papineni-EtAl:2002:ACL"))))
  (testing "from *in*"
    (let [output (with-in-str (-> "examples/multi/paper.bib" io/resource slurp)
                   (with-out-str (cli/-main "cat")))]
      (is (str/includes? output "@article{Lowry01111951"))
      (is (str/includes? output "@InProceedings{papineni-EtAl:2002:ACL")))))

(deftest test-cat
  (let [command-fn (get cli/commands "cat")
        inputs (-> "examples/multi/paper.bib" resource->named-reader list)
        items (command-fn inputs)]
    (is (= 4 (count items)))))

(deftest test-main-cat-remove-fields
  (testing "from filepath"
    (let [output (with-out-str (cli/-main "cat" "test/resources/examples/multi/paper.bib"
                                          "--remove-field" "DOI,eprint"))]
      (is (false? (str/includes? output "DOI")))
      (is (false? (str/includes? output "eprint"))))))

(deftest test-cat-remove-fields
  (let [command-fn (get cli/commands "cat")
        inputs (-> "examples/multi/paper.bib" resource->named-reader list)
        blacklist ["pages" "url"]
        items (command-fn inputs :remove-fields blacklist)
        first-item (first items)]
    (is (false? (str/includes? first-item "pages")))
    (is (false? (str/includes? first-item "URL")))))

(deftest test-cat-select-files
  (let [command-fn (get cli/commands "cat")
        inputs (-> "examples/multi/paper.bib" resource->named-reader list)
        select-files (map resource->named-reader ["examples/multi/paper.aux"
                                                  "examples/multi/paper.tex"])
        items (command-fn inputs :select-files select-files)]
    (is (= 2 (count items)))))

(deftest test-json
  (let [command-fn (get cli/commands "json")
        inputs (-> "examples/multi/paper.bib" resource->named-reader list)
        items (command-fn inputs)]
    (is (= 4 (count items)))
    (is (every? #(str/starts-with? % "{") items))))

(deftest test-json-remove-fields
  (let [command-fn (get cli/commands "json")
        inputs (-> "examples/multi/paper.bib" resource->named-reader list)
        blacklist ["pages" "url"]
        items (command-fn inputs :remove-fields blacklist)
        first-item (first items)]
    (is (false? (str/includes? first-item "pages")))
    (is (false? (str/includes? first-item "URL")))))

(deftest test-test
  (let [command-fn (get cli/commands "test")
        filenames ["examples/multi/paper.bib" "examples/multi/paper.aux"]
        inputs (map resource->named-reader filenames)
        [bib-line aux-line] (command-fn inputs)]
    (is (= "yes" bib-line))
    (is (= "no" aux-line))))

(deftest test-interpolate
  (let [command-fn (get cli/commands "interpolate")
        filenames ["examples/literal/raw.bib" "examples/literal/raw.tex"]
        inputs (map resource->named-reader filenames)
        items (command-fn inputs)]
    (is (= (tex/read-str "C.f. \\citealt{littlemore} wherein \\citet{vital-etal} disagreed \\citep{adamsdonut}.\n")
           (first items)))))
