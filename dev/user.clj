(ns user
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pp pprint print-table]]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
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
            [bartleby.language.json :refer [toJSON fromJSON]]
            [clojure.tools.trace :as trace]
            [clojure.tools.namespace.repl :refer [refresh refresh-all]]))

(println "📕  Loading /dev/user 📖")

(defn read-resource
  [name]
  (->> name
       io/resource
       io/reader
       cli/char-seq
       bibtex/read-all))

(defn debug-loc
  [loc]
  {:path    (pr-str (some-> loc zip/path))
   :depth   (count (some-> loc zip/path))
   :left    (pr-str (some-> loc zip/left zip/node))
   :node    (pr-str (some-> loc zip/node))
   :right   (pr-str (some-> loc zip/right zip/node))
   :next    (pr-str (some-> loc zip/next zip/node))
   :branch? (try (zip/branch? loc) (catch Exception ex :error))
   :end?    (some-> loc (zip/end?))})

(defn lazy-loc-seq [loc]
  (cons loc (when-not (zip/end? loc)
              (lazy-seq (lazy-loc-seq (zip/next loc))))))

(defn print-zip-table
  ([tree]
   ; [:left :node :right :next :branch? :end?]
   (print-zip-table [:end? :depth :node :branch? :next] tree))
  ([ks tree]
   (->> (zip/seq-zip (seq tree))
        (lazy-loc-seq)
        (map debug-loc)
        (print-table ks))))

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
  (println (read-resource "examples/multi/paper.bib"))

  (debug-bibtex "@book{benjamin,  title = {Reason \\emph{and} F\\`ate},}")

  (trace/trace-ns 'clojure.zip)

  (print-zip-table (debug-tex-transformations "\\'{}a"))

  (-> '(:emph (:textbf (\b \o \l \d \+ \i \t \a \l \i \c)))
      zip/seq-zip
      zip/next
      zip/node)

  (debug-tex-transformations "\\'" #'tex/interpret-commands)

  nil)
