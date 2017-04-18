(ns bartleby.bibliography
  (:require [clojure.string :as str]))

(defprotocol ToJSON
  (toJSON [this] "Convert this into a (flat) JSON-friendly structure"))

(defrecord Field [key value]
  ToJSON
  (toJSON [this]
    {key value}))

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

(defn title-case
  "Convert each word in s to title case, using a simple regular expression for detecting words"
  [s]
  (str/replace (str/lower-case s) #"\b(\w)(\w*)"
    (fn [[_ initial remainder]] (str (str/upper-case initial) (str/lower-case remainder)))))

(defn match-case
  "Change the case of s to match the case pattern of prototype.
  Returns s unchanged if no pattern can be inferred from prototype."
  [s prototype]
  (condp = prototype
    (str/lower-case prototype) (str/lower-case s) ; all lower case
    (str/upper-case prototype) (str/upper-case s) ; all upper case
    (str/capitalize prototype) (str/capitalize s) ; Sentence case
    (title-case prototype) (title-case s) ; Title case
    s))

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

(defn- fields-extract-subtitles
  "If there is a colon in a (book)title field, and no existing sub(book)title,
  remove the subtitle from the field's value and put it in a new field"
  [fields]
  (let [existing-keys (->> fields (map :key) (map str/lower-case) set)]
    ; have to flat map over the transformer since splits result in multiple fields
    (mapcat (fn [{:keys [key] :as field}]
              (let [subkey (str/replace key #"(?i)(sub)?title" "subtitle")]
                (if (or (= subkey key) (contains? existing-keys (str/lower-case subkey)))
                  ; this field cannot be split
                  (list field)
                  ; this is a splittable field that has not already been split
                  (split-field field subkey)))) fields)))

(defn extract-subtitles
  "Run item's fields through fields-extract-subtitles if it's a Reference,
  otherwise, return item unchanged"
  [item]
  (cond-> item
    (Reference? item) (update :fields fields-extract-subtitles)))
