(ns bartleby.jats
  (:require [clojure.string :as string]
            [bartleby.core :refer [split-fullname]]
            [clojure.data.xml :refer [emit-str alias-uri sexp-as-element xml-comment]]
            [clojure.data.xml.protocols :refer [AsElements as-elements]])
  (:import [bartleby.bibliography Reference Gloss]))

(def ^:private public-identifier "-//NLM//DTD JATS (Z39.96) Journal Publishing DTD v1.1 20151215//EN")
(def ^:private system-identifier "https://jats.nlm.nih.gov/publishing/1.1/JATS-journalpublishing1.dtd")
(def ^:private doctype (format "<!DOCTYPE article PUBLIC \"%s\" \"%s\">" public-identifier system-identifier))

(def ^:private xlink-ns "http://www.w3.org/1999/xlink")
(alias-uri 'xlink xlink-ns)

(defn- Field->keyval
  [{:keys [key value]}]
  [(string/lower-case key) value])

(extend-protocol AsElements
  Reference
  (as-elements [{:keys [pubtype citekey fields]}]
    (let [{:strs [journal doi title author month year volume issue url keywords]} (mapcat Field->keyval fields)]
      [(sexp-as-element
        [:article {:xmlns/xlink xlink-ns :dtd-version "1.1" :article-type pubtype} ; ::xml/lang "en"
          [:front
            [:journal-meta
              [:journal-title-group
                [:journal-title journal]]]
            [:article-meta
              (when doi [:article-id {:pub-id-type "doi"} doi])
              [:title-group
                [:article-title title]]
              [:contrib-group
                (for [contrib-author (some-> author (string/split #"\s+and\s+"))
                      :let [{:keys [surname given-names]} (split-fullname (string/trim contrib-author))]]
                  [:contrib {:contrib-type "author"}
                    [:name
                      [:surname surname]
                      [:given-names given-names]]
                    [:email ""]])]
              [:pub-date {:date-type "pub"}
                (when month [:month month])
                [:year year]]
              (when volume [:volume volume])
              (when issue [:issue issue])
              (when url [:self-uri {::xlink/href url} "Also available at " url])
              (when keywords
                [:kwd-group
                  (for [kwd (string/split keywords #",")]
                    [:kwd (string/trim kwd)])])]]])]))
  Gloss
  (as-elements [{:keys [lines]}]
    [(xml-comment (string/join \newline lines))]))

(defn write-str
  "generate data.xml elements from expr"
  [entry]
  (->> (as-elements entry)
       (map #(emit-str % :encoding "UTF-8" :doctype (when (instance? Reference entry) doctype)))
       (string/join)))
