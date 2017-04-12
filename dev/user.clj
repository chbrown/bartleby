(ns user
  (:refer-clojure :exclude [char]) ; avoid warning against parsatron overriding (char)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            ; [clojure.repl :refer :all]
            [clojure.tools.namespace.repl :refer [refresh refresh-all]]
            [clojure.test :as test]
            [the.parsatron :as parsatron] ; [the.parsatron :refer :all]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]
            [bartleby.language.common :as common])
  (:gen-class))

(defn -main
  []
  (println "Running :dev user/main"))
