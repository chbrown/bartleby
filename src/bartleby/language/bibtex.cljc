(ns bartleby.language.bibtex
  (:refer-clojure :exclude [char comment read]) ; avoid warning about parsatron overriding (char)
  (:require [clojure.string :as string]
            [the.parsatron :refer :all]
            [bartleby.language.common :refer :all]))

; object model (records)
; ======================

(defprotocol Formattable
  (toString [this options] "Format this into an options-customized string representation"))

(defprotocol ToJSON
  (toJSON [this] "Convert this into a (flat) JSON-friendly structure"))

(defrecord Field [key value]
  Formattable
  ; Helper function for currying the indentation and =-padding options before formatting multiple fields
  (toString [this {:keys [indentation
                          =-padding]
                   :or   {indentation "  "
                          =-padding " "}}]
    (str indentation key =-padding \= =-padding \{ value \}))
  ToJSON
  (toJSON [this]
    {key value}))

(defn split-field
  "If field contains a colon, move the bit after the colon into a new field
  and return both; otherwise return a singleton vector of the original field"
  [field suffix-key]
  (let [{:keys [key value]} field
        [prefix suffix] (string/split value #":" 2)]
    (if suffix
      [(Field. key (string/trimr prefix))
       (Field. suffix-key (string/triml suffix))]
      [field])))

(defrecord Reference [pubtype citekey fields]
  Object
  (toString [this] (toString this {}))
  Formattable
  (toString [this {:keys [remove-fields
                          indentation
                          trailing-comma?
                          trailing-newline?
                          =-padded?]
                   :or   {remove-fields     #{}
                          indentation       "  "
                          trailing-comma?   true
                          trailing-newline? true
                          =-padded?         true}}]
    ; omit citekey (and the comma after) if citekey is nil
    (str \@ pubtype \{ (some-> citekey (str \,)) \newline
         (->> fields
              (remove #(-> % :key string/lower-case remove-fields))
              (map #(toString % {:indentation indentation :=-padding (when =-padded? \space)}))
              (string/join (str \, \newline)))
         (when trailing-comma? \,) (when trailing-newline? \newline)
         \} \newline))
  ToJSON
  (toJSON [this]
    (into {"pubtype" pubtype, "citekey" citekey} (map toJSON fields))))

(def Reference? (partial instance? Reference))

(defrecord Gloss [lines]
  Object
  (toString [this] (toString this nil))
  Formattable
  (toString [this _]
    (string/join \newline lines))
  ToJSON
  (toJSON [this]
    {"lines" lines}))

(defn fromJSON
  [object]
  (if (contains? object "pubtype")
    ; (map->Reference object)
    (let [pubtype (get object "pubtype")
          citekey (get object "citekey")
          fields-map (dissoc object "pubtype" "citekey")
          fields (map (fn [[key value]] (Field. key value)) fields-map)]
      (Reference. pubtype citekey fields))
    ; (map->Gloss object)
    (Gloss. (get object "lines"))))

; parsing
; =======

(def ^:private delimiter-chars (into whitespace-chars #{\, \{ \} \=}))

(defn- until-escapable
  "greedily consume the input until hitting the designated token,
  also consuming escaped instances"
  [c]
  (many
    (choice
      (attempt (string (str "\\" c)))
      (token #(not= % c)))))

(defn word
  "Match any BibTeX-valid naked identifier, like pubtype, citekey, or field key,
  i.e., a contiguous string of anything but whitespace, commas, and end braces."
  []
  (let->> [chars (many1 (any-char-except-in delimiter-chars))]
    (always (apply str chars))))

; tex-chunk and tex-block are mutually recursive
(declare tex-block)

; doesn't work in naked defn form, even when simplified, due to mutual recursion;
; one of them (tex-block or tex-chunk) has to be lazy!
(defparser tex-chunk []
  ; accept any number of things that are not (unescaped) close curly braces
  ; returns a chunk of tex, which is either a string or a single character
  (choice
    (attempt (string "\\{"))
    (attempt (string "\\}")) ; matches same thing as (string [\\ \}]) but has different result
    (any-char-except-in #{\{ \}})
    (curly-braced (tex-block))))

(defn tex-block
  "Reads a block of TeX syntax, recursively, and returns a single string
  enclosed in curly braces"
  []
  (let->> [chunks (many (tex-chunk))]
    (always (str \{ (apply str chunks) \}))))

(defn outer-tex-block
  "Reads a block of TeX syntax, recursively, and returns a single string"
  []
  (let->> [chunks (many (tex-chunk))]
    ; the root block gets special treatment to avoid the outermost braces
    (always (apply str chunks))))

(defn simple-string
  "Read a simple string literal and convert to TeX syntax"
  []
  (let->> [chars (until-escapable \")]
    (let [s (apply str chars)
          ; unescaped quotes
          raw (string/replace s #"\\\"" "\"")
          ; escaped braces
          escaped (string/replace raw #"[{}]" "\\\\$0")]
      (always escaped))))

(defn number-literal
  "Consume one or more digits and return a string in TeX syntax (in curly braces)"
  []
  (let->> [chars (many1 (digit))]
    (always (apply str chars))))

(defn field-value
  "Read the RHS of a field-value field pair, as a string"
  []
  ; should this handle whitespace, or should the field parser (as is currently the case)?
  (choice (curly-braced (outer-tex-block))
          (double-quoted (simple-string))
          ; TODO: handle string variable references
          (number-literal)))

(defn field
  "Use within the outer braces of a BibTeX entry, to capture a single key-value
  field pair and consume the following comma and whitespace, if any."
  []
  (let->> [key (word)
           _ whitespace
           _ (char \=)
           _ whitespace
           value (field-value)
           _ whitespace
           _ (maybe (char \,))
           _ whitespace]
    (always (Field. key value))))

(defn reference
  "Capture a single BibTeX entry, returning a Reference record"
  []
  (let->> [_ (char \@)
           pubtype (word)
           _ (char \{)
           _ whitespace
           citekey (maybe (before (>> whitespace (char \,)) (word)))
           _ whitespace
           fields (many (attempt (field)))
           ; handle entries closed by eof rather than a proper close brace,
           ; which happens quite a bit more than you'd think
           ; perhaps the lookahead wrapper isn't necessary?
           ; was: (lookahead (eof))
           _ (either (char \}) (eof))]
    (always (Reference. pubtype citekey fields))))

(defn gloss-line-characters
  "Captures a single line of interlinear comments,
  i.e., whatever is not a Reference,
  returning a sequence of characters"
  []
  (between (lookahead (any-char-except \@))
           linebreak
           (many (any-char-except-in #{\return \newline}))))

(defn gloss-line
  ; TODO: create a parsastron helper to map a parser's cok/eok values through the given function and merge this wrapper with gloss-line-characters
  []
  (let->> [characters (gloss-line-characters)]
    (always (apply str characters))))

(defn gloss
  []
  (let->> [lines (many1 (gloss-line))]
    (always (Gloss. lines))))

(defn item
  "Capture any valid bibtex item, potentially preceded by whitespace"
  []
  ; reference and gloss very intentionally have no shareable prefix, so no (attempt ...) is needed
  (>> whitespace
      ; TODO: handle special commands like @preamble or @string.
      (choice (reference)
              (gloss))))

; similar to clojure.data.json (http://clojure.github.io/data.json/),
; the primary API consists of the functions read(-str) and write(-str)

(defn read
  "read the first bibliography item from the given stream of BibTeX characters"
  [reader]
  (run (item) reader))

(defn read-str
  "read the first bibliography item from the given string of BibTeX"
  [s]
  ; TODO: convert s to decomplected (generic b/w clj and cljs) reader first?
  (read s))

(defn read-all
  [s]
  (run-seq (item)
           (>> whitespace (eof)) s))

(defn ^String write-str
  ([item] (write-str item {}))
  ([item options]
   (toString item options)))

(defn write
  ([item ^java.io.Writer writer] (write item writer {}))
  ([item ^java.io.Writer writer options]
   (.write writer (write-str item options))))
