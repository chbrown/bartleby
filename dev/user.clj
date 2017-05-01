(ns user
  (:require [clojure.repl :refer :all]
            [clojure.pprint :refer [pp pprint print-table]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [the.parsatron :as parsatron]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.language.tex :as tex]
            [bartleby.bibliography :refer [->Field ->Reference ->Gloss]]
            [clojure.tools.namespace.repl :refer [refresh refresh-all]]))

(println "📕  Loading /dev/user 📖")
