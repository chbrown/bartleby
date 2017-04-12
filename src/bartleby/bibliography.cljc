(ns bartleby.bibliography
  (:require [clojure.string :as str]))

(defprotocol ToJSON
  (toJSON [this] "Convert this into a (flat) JSON-friendly structure"))

(defrecord Field [key value]
  ToJSON
  (toJSON [this]
    {key value}))

(defn split-field
  "If field contains a colon, move the bit after the colon into a new field
  and return both; otherwise return a singleton vector of the original field"
  [field suffix-key]
  (let [{:keys [key value]} field
        [prefix suffix] (str/split value #":" 2)]
    (if suffix
      [(Field. key (str/trimr prefix))
       (Field. suffix-key (str/triml suffix))]
      [field])))

(defrecord Reference [pubtype citekey fields]
  ToJSON
  (toJSON [this]
    (into {"pubtype" pubtype, "citekey" citekey} (map toJSON fields))))

(def Reference? (partial instance? Reference))

(defrecord Gloss [lines]
  ToJSON
  (toJSON [this]
    {"lines" lines}))

(defn fromJSON
  [object]
  (if (contains? object "pubtype")
    ; (map->Reference object)
    (let [pubtype (get object "pubtype")
          citekey (get object "citekey")
          fields-map (dissoc object "pubtype" "citekey")
          fields (map (fn [[key value]] (Field. key value)) fields-map)]
      (Reference. pubtype citekey fields))
    ; (map->Gloss object)
    (Gloss. (get object "lines"))))

(defn remove-fields
  "Remove fields matching fields-to-remove from the reference's list of fields"
  [reference & fields-to-remove]
  (let [blacklist (set fields-to-remove)]
    (update reference :fields (fn [fields]
      (remove #(-> % :key str/lower-case blacklist) fields)))))
