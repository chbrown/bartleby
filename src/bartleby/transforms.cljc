(ns bartleby.transforms
  (:require [clojure.string :as str]
            [bartleby.core :as core]
            [bartleby.bibliography :as bibliography]))

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
                  (bibliography/split-field field subkey)))) fields)))

(defn extract-subtitles
  "Run item's fields through fields-extract-subtitles if it's a Reference,
  otherwise, return item unchanged"
  [item]
  (cond-> item
    (bibliography/Reference? item) (update :fields fields-extract-subtitles)))

(defn- create-throwable
  [^String message]
  #?(:clj (Exception. message)
     :cljs (js/Error. message)))

(defn- create-error-transform
  [name]
  (fn [item]
    (throw (create-throwable (format "Could not find transform \"%s\"" name)))
    item))

(def transforms {"extract-subtitles" extract-subtitles})

(defn compose-transforms-by-name
  "Find the specified transforms by name and combine into one function that
  applies them left to right"
  [names]
  (->> names
       (map #(get transforms % (create-error-transform %)))
       (reverse)
       (apply comp)))
