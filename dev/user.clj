(ns user
  (:refer-clojure :exclude [char]) ; avoid warning against parsatron overriding (char)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            ; [clojure.repl :refer :all]
            [clojure.test :as test]
            [the.parsatron :as parsatron] ; [the.parsatron :refer :all]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]
            ; [bartleby.language.common :as parsers]
            [clojure.tools.namespace.repl :refer [refresh refresh-all]])
  (:gen-class))

(def examplevalue "{Description (in SLF) de la dur{\\'e}e Fran{\\c{c}}aise {\\`a} partir d'une [in French]}")

(defn -main
  []
  (println "Running :dev user/main"))
