(ns bartleby.test-resources
  (:require [clojure.java.io :as io]))

(defmacro read-resource-string
  "Expand to the string contents of the resource designated by `name`"
  [name]
  (slurp (io/resource name)))

(defmacro read-resource-pairs
  "Expand to a mapping from resource name to string contents for files in the test/resources/pairs directory"
  []
  (let [directory (io/file (io/resource "pairs"))
        resource-names (map (fn [child] (str (.getName directory) "/" child)) (.list directory))
        ; is io/resource utf-8 by default? hope so.
        resource-strings (map (fn [name] (slurp (io/resource name))) resource-names)]
    (zipmap resource-names resource-strings)))
