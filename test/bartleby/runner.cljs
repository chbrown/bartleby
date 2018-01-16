(ns bartleby.runner
  (:require [doo.runner :refer-macros [doo-all-tests]]
            [bartleby.test.bibliography]
            [bartleby.test.bibtex]
            [bartleby.test.core]
            ; [bartleby.test.jats]
            [bartleby.test.json]
            [bartleby.test.tex]))

(doo-all-tests)
