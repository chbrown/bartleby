(ns user
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pp pprint print-table]]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.data.xml :as xml]
            [the.parsatron :as parsatron]
            [bartleby.cli :as cli]
            [bartleby.core :as core]
            [bartleby.util :as util]
            [bartleby.bibliography :refer [->Field ->Reference ->Gloss]]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.jats :as jats]
            [bartleby.language.tex :as tex]
            [bartleby.language.texdata :as texdata]
            [bartleby.language.json :refer [toJSON fromJSON]]
            [clojure.tools.trace :as trace]
            [clojure.tools.namespace.repl :refer [refresh refresh-all]]))

(println "📕  Loading /dev/user 📖")

(defn debug-bibtex
  [input]
  (println)
  (println "Input " input)
  (let [item (bibtex/read-str input)]
    (println "Object" (pr-str item))
    (let [output (bibtex/write-str item)]
      (println "Output" output)
      item)))

(defn debug-tex-transformations
  "Parse `input` as TeX, printing the raw input string, the parsed input tree
  representation, and generate a TeX string of that (mostly untransformed) tree.
  Then transform the input tree with `f`, print the transformed tree structure,
  and finally generate the TeX string of that transformed tree.
  Returns the transformed tree."
  [input & fs]
  (println "🔣  Input " (pr-str input))
  (let [tree (tex/read-str input)]
    (println "🌲  Tree  " (pr-str tree))
    (println "🐣  TeX   " (pr-str (tex/write-str tree)))
    (let [transformed-tree (reduce (fn [prev-tree f]
                                     (println "ƒ " (:name (meta f)))
                                     (let [next-tree (f prev-tree)]
                                       (println "🎄  Tree  " (pr-str next-tree))
                                       (println "🐣  TeX   " (pr-str (tex/write-str next-tree)))
                                       next-tree)) tree fs)]
      (println "💥  Output" (pr-str (tex/write-str transformed-tree)))
      transformed-tree)))

(comment
  (println (->> "examples/multi/paper.bib" io/resource io/reader cli/char-seq bibtex/read-all))

  (debug-bibtex "@book{benjamin,  title = {Reason \\emph{and} F\\`ate},}")

  (debug-tex-transformations "\\'" #'tex/interpret-commands)

  nil)
