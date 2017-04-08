(ns bartleby.bibliography
  (:require [clojure.string :as string]))

(defprotocol Formattable
  (toString [this options] "Format this into an options-customized string representation"))

(defprotocol ToJSON
  (toJSON [this] "Convert this into a (flat) JSON-friendly structure"))

(defrecord Field [key value]
  Formattable
  ; Helper function for currying the indentation and =-padding options before formatting multiple fields
  (toString [this {:keys [indentation
                          =-padding]
                   :or   {indentation "  "
                          =-padding " "}}]
    (str indentation key =-padding \= =-padding \{ value \}))
  ToJSON
  (toJSON [this]
    {key value}))

(defn split-field
  "If field contains a colon, move the bit after the colon into a new field
  and return both; otherwise return a singleton vector of the original field"
  [field suffix-key]
  (let [{:keys [key value]} field
        [prefix suffix] (string/split value #":" 2)]
    (if suffix
      [(Field. key (string/trimr prefix))
       (Field. suffix-key (string/triml suffix))]
      [field])))

(defrecord Reference [pubtype citekey fields]
  Object
  (toString [this] (toString this {}))
  Formattable
  (toString [this {:keys [indentation
                          trailing-comma?
                          trailing-newline?
                          =-padded?]
                   :or   {indentation       "  "
                          trailing-comma?   true
                          trailing-newline? true
                          =-padded?         true}}]
    ; omit citekey (and the comma after) if citekey is nil
    (str \@ pubtype \{ (some-> citekey (str \,)) \newline
         (->> fields
              (map #(toString % {:indentation indentation :=-padding (when =-padded? \space)}))
              (string/join (str \, \newline)))
         (when trailing-comma? \,) (when trailing-newline? \newline)
         \} \newline))
  ToJSON
  (toJSON [this]
    (into {"pubtype" pubtype, "citekey" citekey} (map toJSON fields))))

(def Reference? (partial instance? Reference))

(defrecord Gloss [lines]
  Object
  (toString [this] (toString this nil))
  Formattable
  (toString [this _]
    (string/join \newline lines))
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
      (remove #(-> % :key string/lower-case blacklist) fields)))))

(defn ^String write-str
  ([item] (write-str item {}))
  ([item options]
   (toString item options)))

(defn write
  ([item ^java.io.Writer writer] (write item writer {}))
  ([item ^java.io.Writer writer options]
   (.write writer (write-str item options))))
