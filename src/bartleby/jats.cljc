(ns bartleby.jats
  (:require [clojure.string :as string]
            [bartleby.core :refer [split-fullname wrap xml-name]]
            [clojure.data.xml :refer [element xml-comment emit-str]]
            [clojure.data.xml.protocols :refer [AsElements as-elements]])
  (:import (bartleby.bibliography Field Reference Gloss)))

(def ^:private public-identifier "-//NLM//DTD JATS (Z39.96) Journal Publishing DTD v1.1 20151215//EN")
(def ^:private system-identifier "https://jats.nlm.nih.gov/publishing/1.1/JATS-journalpublishing1.dtd")
(def ^:private doctype (format "<!DOCTYPE article PUBLIC \"%s\" \"%s\">" public-identifier system-identifier))

(defn- as-name-element
  [fullname]
  (let [[given-names surname] (split-fullname fullname)]
    (element :name {}
      (element :surname {} surname)
      (element :given-names {} given-names))))

(defn- as-name-elements
  [bibtexnames]
  (->> (string/split bibtexnames #"\s+and\s+")
       (map string/trim)
       (map as-name-element)))

(defn- as-fpage-lpage-elements
  [pages]
  ; split on hyphens, n-dashes, or m-dashes
  (map #(element %1 {} %2) [:fpage :lpage] (string/split pages #"[-–—]+" 2)))

(defn- create-comment
  "pad content with spaces and escape contents if needed"
  [content]
  (-> content
      (string/trim)
      (string/replace #"-{2,}" "–") ; replace any sequences of multiple hyphens with a single n-dash
      (wrap " ")
      (xml-comment)))

; mapping from keywordified Field. :key values to (fn [value] ...element(s)...)
(def ^:private field-mapping {:address #(element :publisher-loc {} %)
                              :author #(element :person-group {:person-group-type "author"} (as-name-elements %))
                              :booktitle #(element :source {} %)
                              :day #(element :day {} %)
                              :doi #(element :pub-id {:pub-id-type "doi"} %)
                              :edition #(element :edition {} %)
                              :editor #(element :person-group {:person-group-type "editor"} (as-name-elements %))
                              :institution #(element :institution {} %)
                              :isbn #(element :isbn {} %)
                              :issn #(element :issn {} %)
                              :issue #(element :issue {} %) ; "issue" is not a legit BibTeX field but whatever
                              :journal #(element :source {} %)
                              :month #(element :month {} %)
                              :note #(element :comment {} %)
                              :number #(element :issue {} %)
                              :page #(as-fpage-lpage-elements %) ; "page" should be "pages" but why not
                              :pages #(as-fpage-lpage-elements %)
                              :publisher #(element :publisher-name {} %)
                              :school #(element :institution {} %)
                              :series #(element :series {} %)
                              :title #(element :article-title {} %)
                              :url #(element :uri {} %)
                              :volume #(element :volume {} %)
                              :year #(element :year {} %)})

(extend-protocol AsElements
  Field
  (as-elements [{:keys [key value]}]
    (list (if-let [value-element (get field-mapping (keyword (string/lower-case key)))]
            (value-element value)
            (create-comment (str key " = " value)))))
  Reference
  (as-elements [{:keys [pubtype citekey fields]}]
    (list (element :ref {:id (xml-name citekey)}
            (element :element-citation {:publication-type pubtype}
              (as-elements fields)))))
  Gloss
  (as-elements [{:keys [lines]}]
    (list (create-comment (string/join \newline lines)))))

(defn write-str
  "Produce JATS XML skeleton with /article/back/ref-list/ref elements"
  [entries]
  ; wrap in root article element
  (-> (element :article {:dtd-version "1.1" :article-type "other"}
        (element :back {}
          (element :ref-list {} (as-elements entries))))
      (emit-str :encoding "UTF-8" :doctype doctype)))
