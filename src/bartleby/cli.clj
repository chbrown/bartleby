(ns bartleby.cli
  (:require [bartleby.bibtex :refer [parse]])
  (:gen-class))

(defn -main
  []
  (println "Running bartleby.cli/main")
  (-> (slurp *in*)
      (parse)
      (println)))
