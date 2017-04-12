(ns bartleby.bibtexml
  (:require [clojure.string :as str]
            [clojure.data.xml :refer [uri-symbol element element* xml-comment]]
            [clojure.data.xml.protocols :refer [AsElements as-elements]])
  (:import (bartleby.bibliography Field Reference Gloss)))

(def ^:private bibtexml-ns "http://bibtexml.sf.net/")

(defn- bibtexml-qname
  "Qualify the given local tag name as a keyword in the bibtexml namespace"
  [local]
  ; (name "Test"), (name 'Test), and (name :Test) all return "Test"
  (keyword (name (uri-symbol bibtexml-ns)) (name local)))

(extend-protocol AsElements
  Field
  (as-elements [{:keys [key value]}]
    [(element (bibtexml-qname (str/lower-case key)) {} value)])
  Reference
  (as-elements [{:keys [pubtype citekey fields]}]
    [(element (bibtexml-qname :entry) {}
      (element* (bibtexml-qname pubtype) {}
        ; clojure.lang.ISeq has an implementation of as-elements as (mapcat as-elements expr)
        (as-elements fields)))])
  Gloss
  (as-elements [{:keys [lines]}]
    [(xml-comment (str/join \newline lines))]))

(defn file-element
  "create a <bibtex:file> data.xml element containing the given entries"
  [entries]
  (element* (bibtexml-qname :file) {:xmlns/bibtex bibtexml-ns} (as-elements entries)))
