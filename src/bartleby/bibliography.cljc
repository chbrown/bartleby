(ns bartleby.bibliography
  (:require [clojure.string :as str]))

(defrecord Field [key value])

(defrecord Reference [pubtype citekey fields])

(defn Reference?
  "Return true if r is an instance of the Reference class"
  [r]
  (instance? Reference r))

(defrecord Gloss [lines])

(defn expand-citekeys
  "Recurse into crossref fields to find all used citekeys"
  [items citekeys]
  (loop [citekeys-set (set citekeys)]
    (let [extended-citekeys-set (->> items
                                     ; find the items indicated by citekeys
                                     (filter #(contains? citekeys-set (:citekey %)))
                                     ; flatten out to selected items' fields
                                     (mapcat :fields)
                                     ; find crossref fields (case-insensitive)
                                     (filter #(= (str/lower-case (:key %)) "crossref"))
                                     ; get field value
                                     (map :value)
                                     (set)
                                     (into citekeys-set))]
      ; if no new citekeys have been added to extended-citekeys-set, we're done
      (if (= citekeys-set extended-citekeys-set)
        extended-citekeys-set
        ; if there are new citekeys in crossref-citekeys, recurse
        (recur extended-citekeys-set)))))

(defn remove-fields
  "Remove all of the fields in reference that have a key that occurs in
  matching-keys, which will be all lower-cased and converted to a set."
  [reference & matching-keys]
  (let [blacklist (set (map str/lower-case matching-keys))
        blacklisted? (fn [field] (contains? blacklist (str/lower-case (:key field))))]
    (update reference :fields (partial remove blacklisted?))))

(defn title-case
  "Convert each word in s to title case, using a simple regular expression for detecting words"
  [s]
  (letfn [(replacer [[_ initial remainder]]
            (str (str/upper-case initial) (str/lower-case remainder)))]
    (str/replace (str/lower-case s) #"\b(\w)(\w*)" replacer)))

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
  "If field's value contains a colon, move the content after the colon into a new field
  and return both; otherwise return a singleton vector of the original field"
  [field suffix-key]
  (let [{:keys [key value]} field
        [prefix suffix] (str/split value #":" 2)]
    (if suffix
      (list (Field. key (str/trimr prefix))
            (Field. suffix-key (str/triml suffix)))
      (list field))))

(defn- fields-extract-subtitles
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
  "If item is an instance of Reference, and there is a colon in a (book)title
  field and no existing (book)subtitle, remove the subtitle from the field's
  value and insert a new field with that value; otherwise, return item unchanged."
  [item]
  (cond-> item
    (Reference? item) (update :fields fields-extract-subtitles)))
