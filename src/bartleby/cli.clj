(ns bartleby.cli
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.tools.cli :refer [parse-opts]]
            [bartleby.core :as core]
            [bartleby.language.bibtex :as bibtex]
            [clojure.java.io :as io])
  (:import [bartleby.core ReadableFile])
  (:gen-class))

(def ^:private properties
  (with-open [reader (-> "META-INF/maven/bartleby/bartleby/pom.properties"
                         io/resource
                         io/reader)]
    (into {} (doto (java.util.Properties.)
                   (.load reader)))))

(def ^:private version
  (get properties "version"))

(defrecord BufferedFileReader [name reader]
  ReadableFile
  (getName [this] name)
  (read [this] (.read reader)))

(defn file-reader
  [name]
  (BufferedFileReader. name (io/reader name)))

(defn- input->citekeys
  [input]
  (condp #(string/ends-with? %2 %1) (:name input)
    ".tex" (-> input :reader slurp core/tex->citekeys)
    ".aux" (-> input :reader slurp core/aux->citekeys)
    []))

(defn- select-cited-from-inputs
  "Select only the cited entries from a bibliography, given a list of .tex and .bib files"
  [inputs]
  (let [bib-inputs (filter #(-> % :name (string/ends-with? ".bib")) inputs)
        items (mapcat #(-> % core/char-seq bibtex/read-all) bib-inputs)
        direct-citekeys (mapcat input->citekeys inputs)
        citekeys (core/expand-citekeys items direct-citekeys)
        ; only keep items that are references that have been cited, or are not references
        keep? (fn [item] (or (nil? (:citekey item)) (contains? citekeys (:citekey item))))]
    (filter keep? items)))

(defn cat-command
  "Parse the input BibTeX file(s) and reformat as a stream of BibTeX, with minimal changes"
  [inputs options]
  (->> inputs
       (map core/char-seq)
       (mapcat bibtex/read-all)
       (map #(bibtex/write-str % options))))

(defn select-command
  "Filter out unused entries, given one or more .bib files and one or more .aux/.tex files"
  [inputs options]
  (->> inputs
       (select-cited-from-inputs)
       (map #(bibtex/write-str % options))))

(defn json-command
  "Parse BibTeX and output each component as JSON"
  [inputs options]
  (->> inputs
       (map core/char-seq)
       (mapcat bibtex/read-all)
       (map #(json/write-str (.toJSON %)))))

(defn json2bib-command
  "Parse JSON-LD and output as standard formatted BibTeX"
  [inputs options]
  (->> (line-seq inputs)
       (map json/read-str)
       (map bibtex/fromJSON)
       (map #(bibtex/write-str % options))))

(defn test-command
  "Test each file in args and output the ones that are not valid BibTeX"
  [inputs options]
  ; TODO: take filenames, and print the name of each unparseable file to STDERR
  (->> inputs
       (map #(if (core/bibtex? %) "yes" "no"))))

; each command should take (inputs options) and return a seq of lines
; the #' reader macro enables access to the function's metadata later
; but doesn't prevent us from calling the function directly as usual
(def commands {:cat #'cat-command
               :select #'select-command
               :json #'json-command
               :json2bib #'json2bib-command
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
   ["-h" "--help"]
   ["-v" "--version"]])

(def ^:private usage-fmt "bartleby %s

Usage: bart [options] command [input [...]] [< input]

Options:
%s

Commands:
%s
")

(defn -main
  [& argv]
  (let [{:keys [options arguments errors summary] :as opts} (parse-opts argv cli-options)
        commands-summary (summarize-commands commands)
        usage (format usage-fmt version summary commands-summary)
        [command & args] arguments
        command-fn (get commands (keyword command))]
    (cond
      (:help options) (do (println usage)
                          (System/exit 0))
      (:version options) (do (println "bartleby" version)
                             (System/exit 0))
      (nil? command) (do (println usage)
                         (println "ArgumentError: you must supply a command")
                         (System/exit 1))
      (nil? command-fn) (do (println usage)
                            (println (str "ArgumentError: unrecognized command '" command "'"))
                            (System/exit 1))
      errors (do (println "Argument Error:" (string/join \newline errors))
                 (System/exit 1)))
    (let [arg-inputs (map file-reader args)
          ; TODO: test for stdin-as-TTY better, e.g., http://stackoverflow.com/a/41576107
          stdin-is-tty? (.ready *in*)
          inputs (if stdin-is-tty?
                   (conj (BufferedFileReader. "/dev/stdin" *in*) arg-inputs)
                   arg-inputs)]
      (->> (command-fn inputs options)
           (interpose (str \newline))
           (map #(.write *out* %))
           (dorun)))
    ; weirdly, System/out doesn't always get automatically flushed
    (.flush *out*)))
