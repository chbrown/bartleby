(ns bartleby.language.tex
  "This module is for parsing strings of (La)TeX code into a tree of data structures.

  There are several kinds of TeX nodes/trees/tokens:
  1. Character: which is the form of the input, read character-by-character
  2. String: which can always be treated as a flattened out sequence of characters
  3. nil: indicating a gap, which should be considered equivalent to the empty string, and thus, vacuous
  4. Group: a record wrapping a collection, indicating that the contents are to be treated as a TeX 'group'
  5. ControlSequence: a record with a single field:
     * `name`: a string (a single non-letter for control symbols)"
  (:refer-clojure :exclude [char read replace])
  (:require [clojure.string :as str]
            [bartleby.util :refer [blank?]]
            [bartleby.language.texdata :refer [whitespace-characters
                                               accent-command->combining-character
                                               command->character]]
            [the.parsatron :refer [run defparser
                                   always attempt between
                                   >> nxt bind
                                   choice either
                                   many many1 times
                                   token char letter?]]))

(defrecord Group [tokens])

(defn Group?
  "Return true if `value` is an instance of the Group class"
  [value]
  (instance? Group value))

(defrecord ControlSequence [name])

;; predicates

(defn- accent-command?
  "A command is an accent command if it matches one of the known accent command strings"
  [command-name]
  (contains? accent-command->combining-character command-name))

(defn- control-word?
  "A command is a control word if it is longer than one character or is a single letter (not a symbol)"
  [command-name]
  (or (> (count command-name) 1)
      (letter? (first command-name))))

;; parsers

(defn- space
  "Consume a single whitespace character"
  []
  (token whitespace-characters))

(defn control-sequence
  "Parse a TeX 'control sequence', which can be a 'control symbol' or a 'control word'.
  Knuth calls them 'control sequences', but Lamport calls them 'commands'.

  - A 'control symbol' is the escape character + another non-letter character.
    Subsequent whitespace is not consumed. Returns a string.
    - One exception is that accent commands ALWAYS absorb space directly after them.
      * In the simplest case, they are followed by a character, and they attach to that character.
      * If they are followed by a block, things get tricky, as they are not allowed to attach to any leading space in the block.
  - A 'control word', i.e., the escape character + a sequence of letter characters.
    All whitespace after the control word is immediately absorbed.

  This parser produces a ControlSequence instance."
  []
  (>> (char \\)
      ; this is a bit tricky since accent commands absorb the space after them,
      ; but some accent command share the same prefix with control words.
      ; further, control-symbol would match some of the accent characters
      (bind (choice (times 1 (token (complement letter?))) ; a single non-letter [^a-zA-Z] character
                    (many1 (token letter?)))
            (fn [characters]
              (let [command-name (str/join characters)]
                ; if command-name is an accent command or a control word...
                (if (or (accent-command? command-name) (control-word? command-name))
                  ; ...absorb subsequent whitespace
                  (>> (many (space)) (always (ControlSequence. command-name)))
                  ; otherwise, after non-accent control symbols, don't absorb whitespace
                  (always (ControlSequence. command-name))))))))

(defn tex-comment
  "Parse a TeX comment. For now, immediately discard the contents and produce nil."
  []
  (>> (char \%)
      ; TODO: handle other linebreaks
      (many (token #(not= % \newline)))
      (char \newline)
      (always nil)))

(defn dash-ligature
  "Parse a dash ligature; i.e., one of: -, --, ---"
  []
  (>> (char \-) (choice
                  (>> (char \-) (choice
                                  (>> (char \-) (always (ControlSequence. "textemdash")))
                                  (always (ControlSequence. "textendash"))))
                  (always \-))))

(defn quoteleft-ligature
  "Parse a left-quote ligature; i.e., one of: `, ``"
  []
  (>> (char \`) (choice
                  (>> (char \`) (always (ControlSequence. "textquotedblleft")))
                  (always (ControlSequence. "textquoteleft")))))

(defn quoteright-ligature
  "Parse a right-quote ligature; i.e., one of: ', ''"
  []
  (>> (char \') (choice
                  (>> (char \') (always (ControlSequence. "textquotedblright")))
                  (always (ControlSequence. "textquoteright")))))

; tex-token and group are mutually recursive, so one has to be declared before the other uses it
(declare tex-token)

(defn group
  "Parse a TeX 'group', starting with an opening curly brace, {,
  recursing as needed, until it hits the matching closing brace, }.
  { and } are synonyms for \\bgroup and \\egroup, respectively.
  Returns a Group instance."
  []
  (bind (between (char \{) (char \}) (many (tex-token)))
        (fn [tokens]
          ; TODO: remove the (seq ...) when parsatron/many (which returns [] for 0 matches rather than nil) is fixed
          (always (Group. (seq tokens))))))

; Return a TeX tree string built by parsing and then generating TeX strings
; in nested parsers, potentially recursively.
(defparser tex-token []
  (choice
    ; we cannot combine control-sequences (aka. commands) with their arguments now,
    ; since the latex compiler is eager, but the parser is patient;
    ; i.e. parsing \textbf\'a bolds the stranded acute accent, not an acute-accented "a"
    (control-sequence)
    (attempt (group)) ; maybe needn't be attempt?
    ; (tex-comment) ; bibtex uses percent-encoded URLs but this should be enabled in non-bibtex contexts
    ; Handle ligatures: plain characters that get tranformed into non-ASCII
    ; when rendered; see Chapter 9 of The TeX Book, page 51.
    ; --- and -- and -
    (dash-ligature)
    ; TODO: maybe handle quotes differently somehow in math environments?
    ; `` and `
    (quoteleft-ligature)
    ; '' and '
    (quoteright-ligature)
    (attempt (>> (char \!) (char \`) (always (ControlSequence. "textexclamdown"))))
    (attempt (>> (char \?) (char \`) (always (ControlSequence. "textquestiondown"))))
    ; not technically a ligature:
    (>> (char \~) (always \u00A0)) ; NO-BREAK SPACE (U+00A0)
    ; TODO: handle other escaped things?
    ; parse anything (and everything) else as a raw character, except for },
    ; which we have to fail on so that groups can parse it
    (token #(not= % \}))))

(defn tex-document
  "Parse a sequence of TeX tokens as a Group instance.
  Unlike the (group) parser, this does not require braces."
  []
  (bind (many (tex-token))
        (fn [tokens] (always (Group. (seq tokens))))))

; (defn read
;   "Parse and simplify the TeX reader into a string of TeX"
;   [reader]
;   (run (tex-document) reader))

(defn read-str
  "Parse a TeX string into a sequence of TeX tokens, returning a Group instance."
  [string]
  (run (tex-document) string))

;; writer

(def ^{:dynamic true :private true} *root* true)
(def ^{:dynamic true :private true} *flatten*)

(defprotocol TeXWriter
  "Format TeX nodes"
  (-write-str [this]
    "Format the TeX data structure, `this`, returning a string."))

(extend-protocol TeXWriter
  Group
  (-write-str [group]
    ; we always suppress braces at the root, and lower down if *flatten* is true
    (let [braces? (when-not *root* (not *flatten*))]
      ; anything lower down is not root
      (binding [*root* false]
        (str (when braces? \{)
             (str/join (map -write-str (:tokens group)))
             (when braces? \})))))
  ControlSequence
  (-write-str [control-sequence]
    (str \\ (:name control-sequence)))
  #?(:clj Object :cljs default) ; e.g., Character or String/string
  (-write-str [value]
    value)
  nil
  (-write-str [_]
    nil))

(defn write-str
  "Convert TeX data structure into TeX-formatted string"
  [item & options]
  (let [{:keys [flatten]
         :or   {flatten false}} options]
    (binding [*flatten* flatten]
      (-write-str item))))

(defn write
  "Write TeX-formatted output to a java.io.Writer."
  [tree ^java.io.Writer writer & options]
  (.write writer ^String (apply write-str tree options)))

;; manipulations

(defn join
  "Return a new TeX sequence that is the concatenation of the given TeX sequences,
  separated by separator (a list of TeX tokens), if given."
  ([documents]
   (Group. (mapcat :tokens documents)))
  ([separator documents]
   (Group. (apply concat (interpose separator (map :tokens documents))))))

(defn split
  "Split `document` into two at the first occurrence of `separator` at the root level.
  Returns a sequence of a single Group if `separator` is not found."
  [document separator]
  {:pre [(instance? Group document)
         (char? separator)]}
  (let [[prefix [_ & suffix]] (split-with #(not= separator %) (:tokens document))]
    (cons (Group. prefix) (when suffix (list (Group. suffix))))))

(defn triml
  [document]
  {:pre [(instance? Group document)]}
  (update document :tokens #(drop-while blank? %)))

(defn trimr
  [document]
  {:pre [(instance? Group document)]}
  ; reverse; drop from left-to-right; then reverse back to the original order
  ; TODO: is there a smarter way to do this?
  (update document :tokens #(->> % reverse (drop-while blank?) reverse)))

(defn- starts-with?
  "True if `coll` starts with `subcoll`"
  [coll subcoll]
  (= (take (count subcoll) coll) subcoll))

(defn- index-of-sublist
  "Return the position of `target` within `source`,
  or nil if `target` cannot be found."
  ; thanks to https://stackoverflow.com/a/15224555
  [source target]
  {:pre [(coll? source) (coll? target)]}
  #?(:clj  (let [index (java.util.Collections/indexOfSubList source target)]
             (when (not= index -1)
               index))
     :cljs (let [max-possible-index (- (count source) (count target))]
             (when-not (neg? max-possible-index)
               (->> (range (inc max-possible-index))
                    (filter (fn [idx] (starts-with? (drop idx source) target)))
                    (first))))))

(defn replace
  "Search for `pattern` within `document` (within the root-level tokens), replacing via `replacement` if found.
  `replacement` can be a function (which will be called with subsequence matched by `pattern`) or a literal value."
  ; TODO: search at all levels, not just the root.
  [document pattern-tokens replacement]
  {:pre  [(instance? Group document)
          (coll? pattern-tokens)
          (or (coll? replacement) (fn? replacement))]
   :post [(instance? Group %)]}
  (loop [tokens (:tokens document)]
    (if-let [index-of-pattern (index-of-sublist tokens pattern-tokens)]
      (let [[prefix suffix] (split-at index-of-pattern tokens)
            ; `prefix` is everything up until `pattern-tokens`
            ; `suffix` starts with `pattern-tokens`
            replacement-value (if (fn? replacement)
                                (replacement (take (count pattern-tokens) suffix))
                                replacement)]
        (recur (concat prefix replacement-value (drop (count pattern-tokens) suffix))))
      (Group. tokens))))

;; interpretation

(defn- command->arity
  "Return the positive integer arity of the command indicated by `command-name` (a string)"
  [command-name]
  {:pre  [(string? command-name)]
   :post [(integer? %)]}
  (cond
    (contains? command->character command-name) 0
    (accent-command? command-name) 1
    ; TODO: look it up in a contextual store somehow
    :else 1))

(defn- interpret-accent-command
  "Find the combining character and put it after the argument.
  Returns a raw sequence of TeX tokens."
  [command-name [argument & more]]
  {:pre [(nil? more) ; the second argument can be 0- or 1-long, but no longer
         (not (sequential? argument))]} ; the argument can be nil, a Group instance, or a primitive token
  (let [combining-character (accent-command->combining-character command-name)
        ; default argument to an empty group if missing
        argument (or argument (Group. nil))]
    (if (instance? Group argument)
      (let [tokens (:tokens argument)]
        (if (empty? tokens)
          ; if the given argument is empty, fabricate a space to combine with
          (list \space combining-character)
          ; else, dig in to find a viable target
          (let [tokens (drop-while blank? tokens)]
            (if (char? (first tokens))
              ; okay, put it after that
              (list* (first tokens) combining-character (rest tokens))
              ; otherwise, use a dummy space to stick it right here
              (list* \space combining-character tokens)))))
      ; argument is a single non-nil value, probably a character;
      ; TODO: if it's nil, e.g. due to \relax, is that the caller's problem?
      (list argument combining-character))))

(defn- interpret-command
  "Returns a sequence of TeX tokens replacing the command"
  [command-name arguments]
  (cond
    ; if we've got a simple command -> character substitution, do it
    (contains? command->character command-name) (list (command->character command-name))
    ; interpreting accent commands is a bit tricky, so we jump off to a separate function
    (accent-command? command-name) (interpret-accent-command command-name arguments)
    ; other control symbols are replaced with their literal name
    (not (control-word? command-name)) (seq command-name)
    ; otherwise, return the interpreted arguments, and drop the command-name
    :else arguments))

(defprotocol TeXCompiler
  "Compile/interpret TeX nodes"
  (interpret-commands [this]
    "Replace fancy characters and unescape accents."))

(defn- interpret-tokens
  "Interpret `tokens` as a sequence of TeX tokens.
  The argument parser depends on the arity of the given command;
  e.g. \\o is 0-argument command, so {\\o} is fine.
       but since \\textbf is a 1-argument command, {\\textbf} is a parse error.
  Takes a sequence and returns a sequence.
  This processes nested values before wider/outer ones."
  [tokens]
  (lazy-seq
    (when-let [token (first tokens)]
      (if (instance? ControlSequence token)
        ; if it's a control-sequence, get its name, collect its arguments, and interpret them
        (let [command-name (:name token)
              arity (command->arity command-name)
              ; TODO: this needs to fail immediately here if (take arity ...)
              ; returns a seq of less than length arity
              argument-tokens (take arity (rest tokens))
              ; interpret each of the given arguments before we get to them
              arguments (map interpret-commands argument-tokens)]
          ; now pack together the result of the command and its arguments we just captured
          (concat (interpret-command command-name arguments)
                  ; and continue with the sequence following all that
                  (interpret-tokens (drop arity (rest tokens)))))
        ; otherwise we process it directly and continue on
        (cons (interpret-commands token)
              (interpret-tokens (rest tokens)))))))

(extend-protocol TeXCompiler
  ; process the contents of a Group (but it always remains a Group instance)
  Group
  (interpret-commands [group]
    (update group :tokens interpret-tokens))
  ; interpret a lone command; commands that are used in a proper sequence won't end up here,
  ; only commands that are a naked argument to another command (which is atypical)
  ControlSequence
  (interpret-commands [control-sequence]
    ; TODO: come up with better solution than this dummy Group.
    (Group. (interpret-command (:name control-sequence) nil)))
  ; identity for everything else (primitive values)
  #?(:clj Object :cljs default)
  (interpret-commands [value]
    value)
  nil
  (interpret-commands [_]
    nil))
