(ns bartleby.language.jats
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [bartleby.util :refer [split-fullname]]
            [bartleby.language.tex :as tex]
            ; must require 'bartleby.bibliography here; otherwise cloverage breaks the AsElements protocol extensions
            [bartleby.bibliography #?@(:cljs [:refer [Field Reference Gloss]])]
            [clojure.data.xml :as xml :refer [element* element]]
            [clojure.data.xml.protocols :refer [AsElements as-elements]])
  #?(:clj (:import (bartleby.bibliography Field Reference Gloss))))

(def ^:private public-identifier "-//NLM//DTD JATS (Z39.96) Journal Publishing DTD v1.1 20151215//EN")
(def ^:private system-identifier "https://jats.nlm.nih.gov/publishing/1.1/JATS-journalpublishing1.dtd")
(def ^:private doctype (str "<!DOCTYPE article PUBLIC \"" public-identifier "\" \"" system-identifier "\">"))

(defn- wrap
  "Wrap the string s in left and right padding"
  ([s both] (wrap s both both))
  ([s left right] (str left s right)))

(defn- xml-name
  "Sanitize the string s into a valid XML name.
  * Prefix with underscore if the first character is not a valid first character.
  * Remove any non- letter/number/some punctuation characters."
  [s]
  (-> s
      (str/replace #"^[^A-Za-z_:]" "_$0")
      (str/replace #"[^A-Za-z0-9._:-]" "")))

(defn- as-name-element
  [fullname]
  (let [[given-names surname] (split-fullname fullname)]
    (element :name {}
      (element :surname {} surname)
      (element :given-names {} given-names))))

(defn- as-name-elements
  [bibtexnames]
  (->> (str/split bibtexnames #"\s+and\s+")
       (map str/trim)
       (map as-name-element)))

(defn- as-fpage-lpage-elements
  [pages]
  ; split on hyphens, n-dashes, or m-dashes
  (map #(element %1 {} %2) [:fpage :lpage] (str/split pages #"[-–—]+" 2)))

(defn- create-comment
  "pad content with spaces and escape contents if needed"
  [content]
  (-> content
      (str/trim)
      (str/replace #"-{2,}" "–") ; replace any sequences of multiple hyphens with a single n-dash
      (wrap " ")
      (xml/xml-comment)))

(def pubtype-mapping
  "Mapping from string pubtype (aka. BibTeX entry type) to one of these
  commonly used types: journal, book, confproc, other"
  {:article       :journal
   :book          :book
   :booklet       :book
   :conference    :confproc
   :inbook        :book
   :incollection  :book
   :inproceedings :confproc
   :manual        :other
   :mastersthesis :other
   :misc          :other
   :phdthesis     :other
   :proceedings   :confproc
   :techreport    :other
   :unpublished   :other})

(def field-mapping
  "Mapping from keywordified Field. :key values to (fn [value] ...element(s)...)"
  {:address     #(element :publisher-loc {} %)
   :author      #(element :person-group {:person-group-type "author"} (as-name-elements %))
   :booktitle   #(element :source {} %)
   :day         #(element :day {} %)
   :doi         #(element :pub-id {:pub-id-type "doi"} %)
   :edition     #(element :edition {} %)
   :editor      #(element :person-group {:person-group-type "editor"} (as-name-elements %))
   :institution #(element :institution {} %)
   :isbn        #(element :isbn {} %)
   :issn        #(element :issn {} %)
   :issue       #(element :issue {} %) ; "issue" is not a legit BibTeX field but whatever
   :journal     #(element :source {} %)
   :month       #(element :month {} %)
   :note        #(element :comment {} %)
   :number      #(element :issue {} %)
   :page        #(as-fpage-lpage-elements %) ; "page" should be "pages" but why not
   :pages       #(as-fpage-lpage-elements %)
   :publisher   #(element :publisher-name {} %)
   :school      #(element :institution {} %)
   :series      #(element :series {} %)
   :title       #(element :article-title {} %)
   :url         #(element :uri {} %)
   :volume      #(element :volume {} %)
   :year        #(element :year {} %)})

(defn- get-from-mapping
  "Convert k to a string, lower-case it, convert it to a keyword, and find it in m.
  Useful in combination with pubtype-mapping and field-mapping for handling BibTeX's case-insensitive strings."
  [m k]
  (->> (name k) (str/lower-case) (keyword) (get m)))

(extend-protocol AsElements
  Field
  (as-elements [{:keys [key value]}]
    (let [create-value-element (get-from-mapping field-mapping key)
          value-string (-> value tex/interpret-commands (tex/write-str :flatten true))]
      (list (if create-value-element
              (create-value-element value-string)
              (create-comment (str key " = " value-string))))))
  Reference
  (as-elements [{:keys [pubtype citekey fields]}]
    (list (element :ref {:id (xml-name citekey)}
            (element :element-citation {:publication-type (name (get-from-mapping pubtype-mapping pubtype))}
              (mapcat as-elements fields)))))
  Gloss
  (as-elements [{:keys [lines]}]
    (list (create-comment (str/join \newline lines)))))

(defn- find-or-append
  "Go through the children of `parent` (a zipper location) in order and find
  the first loc that matches pred, or insert not-found and return its loc.
  We have to use the parent, not the first child, since the element may be empty."
  [parent pred not-found]
  (loop [loc (zip/down parent)]
    (if loc
      (if (pred loc)
        loc ; found!
        (recur (zip/right loc))) ; keep going
      ; otherwise, we're at the end with no match; append and go into the new loc
      (-> parent (zip/append-child not-found) zip/down zip/rightmost))))

(defn- loc-tag=
  "Returns a predicate that takes a zipper loc and returns true
  if that loc's value's tag is equal to `tag`"
  [tag]
  (fn tag=?
    [loc]
    (= tag (:tag (zip/node loc)))))

(defn set-article-refs
  "Find the /article/back/ref-list element in article
  and add all of `entries` as refs (by converting with as-elements) to it"
  [article entries]
  (-> (or article (element :article {:dtd-version "1.1"}))
      (zip/xml-zip)
      (find-or-append (loc-tag= :back) (element :back))
      (find-or-append (loc-tag= :ref-list) (element :ref-list))
      (zip/replace (element* :ref-list nil (as-elements entries)))
      (zip/root)))

(defn write-str
  "Generate XML string with JATS doctype"
  [e]
  (xml/emit-str e :encoding "UTF-8" :doctype doctype))

#?(:clj
   (defn write
     "Write JATS XML with JATS doctype"
     [e ^java.io.Writer writer]
     (xml/emit e writer :encoding "UTF-8" :doctype doctype)))
