(ns bartleby.cli
  (:require [clojure.string :as string]
            [clojure.data.json :as json :refer [JSONWriter]]
            [clojure.data.xml :as xml]
            [clojure.tools.cli :refer [parse-opts]]
            [bartleby.core :as core]
            [bartleby.transforms :refer [compose-transforms-by-name]]
            [bartleby.language.bibtex :as bibtex]
            [bartleby.bibtexml :as bibtexml]
            [bartleby.bibliography :as bibliography]
            [clojure.java.io :as io])
  (:import [bartleby.core ReadableFile]
           [bartleby.bibliography ToJSON])
  (:gen-class))

(extend-type ToJSON
  JSONWriter
  (-write [this out]
    (json/write (bibliography/toJSON this) out)))

(defn- resource->Properties
  "Load the given resource as a Properties instance"
  [resource-name]
  (with-open [reader (-> resource-name io/resource io/reader)]
    (doto (java.util.Properties.)
          (.load reader))))

(defrecord BufferedFileReader [name ^java.io.Reader reader]
  ReadableFile
  (getName [this] name)
  (read [this] (.read reader)))

(defn file-reader
  [name]
  (BufferedFileReader. name (io/reader name)))

(defn- input->items
  [{:keys [name reader]}]
  (when (string/ends-with? name ".bib")
    (-> reader core/char-seq bibtex/read-all)))

(defn- input->citekeys
  [{:keys [name reader]}]
  (cond
    (string/ends-with? name ".tex") (-> reader slurp core/tex->citekeys)
    (string/ends-with? name ".aux") (-> reader slurp core/aux->citekeys)
    :default []))

(defn- select-cited-from-inputs
  "Select only the cited entries from a bibliography, given a list of .tex and .bib files"
  [inputs]
  (let [items (mapcat input->items inputs)
        direct-citekeys (mapcat input->citekeys inputs)
        citekeys (core/expand-citekeys items direct-citekeys)
        ; only keep items that are references that have been cited, or are not references
        keep? (fn [item] (or (nil? (:citekey item)) (contains? citekeys (:citekey item))))]
    (filter keep? items)))

(defn cat-command
  "Parse the input BibTeX file(s) and reformat as a stream of BibTeX, with minimal changes"
  [inputs & options]
  (let [{:keys [transforms remove-fields]} options]
    (->> inputs
         (map core/char-seq)
         (mapcat bibtex/read-all)
         (map #(apply bibliography/remove-fields % remove-fields))
         (map (compose-transforms-by-name transforms))
         (map #(apply bibtex/write-str % options)))))

(defn select-command
  "Filter out unused entries, given one or more .bib files and one or more .aux/.tex files"
  [inputs & options]
  (let [{:keys [remove-fields]} options]
    (->> inputs
         (select-cited-from-inputs)
         (map #(apply bibliography/remove-fields % remove-fields))
         (map #(apply bibtex/write-str % options)))))

(defn json-command
  "Parse BibTeX and output each component as JSON"
  [inputs & options]
  (let [{:keys [remove-fields]} options]
    (->> inputs
         (map core/char-seq)
         (mapcat bibtex/read-all)
         (map #(apply bibliography/remove-fields % remove-fields))
         (map json/write-str))))

(defn json2bib-command
  "Parse JSON-LD and output as standard formatted BibTeX"
  [inputs & options]
  (let [{:keys [remove-fields]} options]
    (->> (line-seq inputs)
         (map json/read-str)
         (map bibliography/fromJSON)
         (map #(apply bibliography/remove-fields % remove-fields))
         (map #(apply bibtex/write-str % options)))))

(defn bibtexml-command
  "Parse BibTeX and output each component as (BibTe)XML"
  [inputs & options]
  (let [{:keys [remove-fields]} options
        root (->> inputs
                  (map core/char-seq)
                  (mapcat bibtex/read-all)
                  (map #(apply bibliography/remove-fields % remove-fields))
                  (bibtexml/file-element))]
    (list (xml/emit-str root :encoding "UTF-8"))))

(defn test-command
  "Test each file in args and output the ones that are not valid BibTeX"
  [inputs & options]
  ; TODO: take filenames, and print the name of each unparseable file to STDERR
  (->> inputs
       (map core/char-seq)
       (map #(if (core/bibtex? %) "yes" "no"))))

(defn interpolate-command
  "Replace literal names with cite commands, given .tex and .bib file(s)"
  [inputs & options]
  (let [input-tex? (fn [{:keys [name reader]}] (or (= name "/dev/stdin") (string/ends-with? name ".tex")))
        items (mapcat input->items inputs)
        references (filter :citekey items)]
    (->> inputs
         (filter input-tex?)
         (map :reader)
         (map slurp)
         (map #(core/interpolate % references)))))

; each command should take (inputs & options) and return a seq of lines
; the #' reader macro enables access to the function's metadata later
; but doesn't prevent us from calling the function directly as usual
(def commands {:cat #'cat-command
               :select #'select-command
               :interpolate #'interpolate-command
               :json #'json-command
               :json2bib #'json2bib-command
               :bibtexml #'bibtexml-command
               :test #'test-command})

(defn- summarize-commands
  [commands]
  (let [command-name-max-length (->> commands keys (map name) (map count) (apply max))
        command-fmt (str "  %-" command-name-max-length "s %s")]
    (->> (for [[command-key command-fn] commands]
           (format command-fmt (name command-key) (:doc (meta command-fn))))
         (string/join \newline))))

(def cli-options
  [[nil "--trailing-comma" "Include optional trailing comma after last field"
    :id :trailing-comma?
    :default true]
   [nil "--no-trailing-comma" "Omit optional trailing comma after last field"
    :id :trailing-comma?
    :parse-fn not]
   [nil "--trailing-newline" "Include optional trailing before entry closing brace"
    :id :trailing-newline?
    :default true]
   [nil "--no-trailing-newline" "Omit optional trailing before entry closing brace"
    :id :trailing-newline?
    :parse-fn not]
   [nil "--padding" "Include space padding around '=' field separator"
    :id :=-padded?
    :default true]
   [nil "--no-padding" "Omit space padding around '=' field separator"
    :id :=-padded?
    :parse-fn not]
   [nil "--remove-field KEY" "Omit fields from output (case-insensitively); this argument can be repeated"
    :id :remove-fields
    :default #{}
    ; downcase (and split on ",", which is not documented for now)
    :parse-fn #(-> % string/lower-case (string/split #","))
    ; support multiple applications
    :assoc-fn (fn [m k v] (update m k into v))]
   ["-t" "--transform NAME" "Transform entries with operation NAME; this argument can be repeated"
    :id :transforms
    :default []
    :assoc-fn (fn [m k v] (update m k conj v))]
   ["-h" "--help"]
   ["-v" "--version"]])

(defn- exit!
  "Wrap System/exit as a Clojure var so that we can mock it out during testing"
  [code]
  (System/exit code))

(defn- print-info-and-exit!
  [summary show-help & messages]
  (let [properties (resource->Properties "META-INF/maven/bartleby/bartleby/pom.properties")
        version (get properties "version")]
    ; always print the 'bartleby <version>' banner
    (println "bartleby" version)
    ; print the full usage + options + commands help when show-help is true
    (when show-help
      (println)
      (println "Usage: bart [options] command [input [...]] [< input]")
      (println)
      (println "Options:")
      (println summary)
      (println)
      (println "Commands:")
      (println (summarize-commands commands))
      (println))
    ; print any given (error) messages
    (doseq [message messages]
      (println message))
    ; exit explicitly & immediately
    (exit! (if messages 1 0))))

(defn -main
  [& argv]
  (let [{:keys [options arguments errors summary] :as opts} (parse-opts argv cli-options)
        [command & args] arguments
        command-fn (get commands (keyword command))]
    (cond
      (:help options) (print-info-and-exit! summary true)
      (:version options) (print-info-and-exit! summary false)
      (nil? command) (print-info-and-exit! summary true "ArgumentError: you must supply a command")
      (nil? command-fn) (print-info-and-exit! summary true (str "ArgumentError: unrecognized command '" command "'"))
      ; clojure.tools.cli/parse-opts may include a vector of error message strings in :errors
      ; if the cli parser encountered any errors (nil implies success)
      errors (print-info-and-exit! summary true (str "Argument Error: " (string/join \newline errors)))
      :default (let [arg-inputs (map file-reader args)
                     ; TODO: test for stdin-as-TTY better, e.g., http://stackoverflow.com/a/41576107
                     stdin-is-tty? (.ready ^java.io.Reader *in*)
                     inputs (if stdin-is-tty?
                              (conj (BufferedFileReader. "/dev/stdin" *in*) arg-inputs)
                              arg-inputs)]
                 (->> options
                      ; convert options map to list of key-value tuples
                      (apply concat)
                      ; command-fn takes options as rest-args
                      (apply command-fn inputs)
                      (interpose (str \newline))
                      (map (fn [^String line] (.write *out* line)))
                      (dorun))))
    ; weirdly, System/out doesn't always get automatically flushed
    (.flush *out*)))
