(ns bartleby.bibliography
  (:require [clojure.string :as str]
            [bartleby.language.tex :as tex]))

; Field attributes:
; * `key` is a string
; * `value` is a TeX structure (usually a sequence of characters)
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
  {:pre [(every? string? citekeys)]}
  (loop [citekeys-set (set citekeys)]
    (let [extended-citekeys-set (->> items
                                     ; limit to references (the subsequent filter implicitly does this too)
                                     (filter Reference?)
                                     ; find the items indicated by citekeys
                                     (filter #(contains? citekeys-set (:citekey %)))
                                     ; flatten out to selected items' fields
                                     (mapcat :fields)
                                     ; find crossref fields (case-insensitive)
                                     (filter #(= (str/lower-case (:key %)) "crossref"))
                                     ; get field value
                                     (map :value)
                                     (map tex/write-str)
                                     (set)
                                     (into citekeys-set))]
      ; if no new citekeys have been added to extended-citekeys-set, we're done
      (if (= citekeys-set extended-citekeys-set)
        extended-citekeys-set
        ; if there are new citekeys in crossref-citekeys, recurse
        (recur extended-citekeys-set)))))

(defn filter-fields
  "Include only the fields in reference that have a key that occurs in
  matching-keys, using case-insensitive matching."
  [reference & matching-keys]
  (let [whitelist (set (map str/lower-case matching-keys))
        whitelisted? (fn [field] (contains? whitelist (str/lower-case (:key field))))]
    (update reference :fields #(filter whitelisted? %))))

(defn remove-fields
  "Remove all of the fields in reference that have a key that occurs in
  matching-keys, using case-insensitive matching."
  [reference & matching-keys]
  (let [blacklist (set (map str/lower-case matching-keys))
        blacklisted? (fn [field] (contains? blacklist (str/lower-case (:key field))))]
    (update reference :fields #(remove blacklisted? %))))

(defn title-case
  "Convert each word in s to title case, using a simple regular expression for detecting words"
  [s]
  (str/replace (str/lower-case s) #"\b(\w)(\w*)"
               (fn replacer [[_ initial remainder]]
                 (str (str/upper-case initial) (str/lower-case remainder)))))

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
        [prefix suffix] (tex/split value \:)]
    (if suffix
      (list (Field. key (tex/trimr prefix))
            (Field. suffix-key (tex/triml suffix)))
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

(defn combine-fields
  "Return a single field that is the concatenation of field's and subfield's
  values, separated by a colon, updating field if possible, or inferring the key
  if only subfield is supplied."
  [field subfield]
  (let [{subkey :key, subvalue :value} subfield]
    (if field
      (update field :value #(tex/join (list \: \space) (list % subvalue)))
      ; weird to have a subtitle but no title, but we'll do our best
      (Field. (str/replace subkey #"(?i)sub" "") subvalue))))

(defn- fields-embed-subtitles
  "For any field matching /.*subtitle/, replace it and its companion with a single field"
  [fields]
  (let [subfields (filter #(re-matches #"(?i).*subtitle" (:key %)) fields)]
    (reduce (fn [fields subfield]
              (let [subkey (:key subfield)
                    ; find companion field(s)
                    companionkey-lower (str/lower-case (str/replace subkey #"(?i)sub" ""))
                    ; use the first companion field found
                    companion (first (filter #(= (str/lower-case (:key %)) companionkey-lower) fields))]
                ; add combined field to the other fields
                (cons (combine-fields companion subfield)
                      (remove #(contains? #{(:key companion) subkey} (:key %)) fields)))) fields subfields)))

(defn embed-subtitles
  "If item is an instance of Reference, and there are any sub(book)title
  fields, remove the subtitle field and  from the field's
  value and insert a new field with that value; otherwise, return item unchanged."
  [item]
  (cond-> item
    (Reference? item) (update :fields fields-embed-subtitles)))
