(ns bartleby.language.json
  (:require [bartleby.language.tex :as tex]
            [bartleby.bibliography])
  (:import (bartleby.bibliography Field Reference Gloss)))

(defprotocol ToJSON
  "Complex data structures to be transformed before serializing as JSON should
  implement this protocol to facilitate cross-platform use.
  Then, in platform-dependent code, this protocol can be extended to implement
  that platform's JSON serialization protocol (e.g., JSONWriter)."
  (toJSON [this] "Convert this into a (flat) JSON-friendly structure"))

(extend-protocol ToJSON
  Field
  (toJSON [{:keys [key value]}]
    {key (-> value tex/simplify tex/write-str)})
  Reference
  (toJSON [{:keys [pubtype citekey fields]}]
    (into {"pubtype" pubtype, "citekey" citekey} (map toJSON fields)))
  Gloss
  (toJSON [{:keys [lines]}]
    {"lines" lines}))

(defn fromJSON
  "Convert object into an instance of either Reference or Gloss,
  depending on whether they key \"pubtype\" is present in object"
  [object]
  (if (contains? object "pubtype")
    (let [pubtype (get object "pubtype")
          citekey (get object "citekey")
          fields-map (dissoc object "pubtype" "citekey")
          fields (map (fn [[key value]] (Field. key [value])) fields-map)]
      (Reference. pubtype citekey fields))
    (Gloss. (get object "lines"))))
