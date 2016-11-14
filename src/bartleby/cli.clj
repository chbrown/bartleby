(ns bartleby.cli
  (:require [bartleby.bibtex :as bibtex])
  (:gen-class))

(defn -main
  []
  (println "Running bartleby.cli/main")
  (-> (slurp *in*)
      (bibtex/bibtex->bibtex)
      (println)))
