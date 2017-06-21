(ns user
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pp pprint print-table]]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [the.parsatron :as parsatron]
            [bartleby.cli :as cli]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.common :as common]
            [bartleby.language.tex :as tex]
            [bartleby.language.json :refer [toJSON fromJSON]]
            [bartleby.bibliography :refer [->Field ->Reference ->Gloss]]
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
