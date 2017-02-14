(ns bartleby.transforms
  (:require [clojure.string :as string]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]))

(def ^:private key->subkey {"title" "subtitle"
                            "booktitle" "booksubtitle"
                            ; maybe handle case-matching better somehow?
                            "Title" "Subtitle"
                            "Booktitle" "Booksubtitle"})

(defn- fields-extract-subtitles
  "If there is a colon in a (book)title field, and no existing sub(book)title,
  remove the subtitle from the field's value and put it in a new field"
  [fields]
  (let [existing-keys (->> fields (map :key) (map string/lower-case) set)]
    (->> (for [field fields
               :let [subkey (key->subkey (:key field))]]
           (if (and subkey (not (existing-keys (string/lower-case subkey))))
             ; this is a splittable field that has not already been split
             (bibtex/split-field field subkey)
             ; this field cannot be split
             [field]))
         ; have to flat map over the fields
         (apply concat))))

(defn extract-subtitles
  "Run item's fields through fields-extract-subtitles if it's a Reference,
  otherwise, return item unchanged"
  [item]
  (if (bibtex/Reference? item)
    (update item :fields fields-extract-subtitles)
    item))

(defn- create-throwable
  [message]
  #?(:clj (Exception. message)
     :cljs (js/Error. message)))

(defn- create-error-transform
  [name]
  (fn [item]
    (throw (create-throwable (format "Could not find transform \"%s\"" name)))
    item))

(def transforms {"extract-subtitles" extract-subtitles})

(defn compose
  "Find the specified transforms by name and combine into one function that
  applies them left to right"
  [names]
  (->> names
       (map #(get transforms % (create-error-transform %)))
       (reverse)
       (apply comp)))
