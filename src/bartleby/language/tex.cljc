(ns bartleby.language.tex
  (:refer-clojure :exclude [char read])
  (:require [clojure.string :as str]
            [the.parsatron :refer :all]
            [bartleby.language.common :refer [whitespace-chars any-char-except-in curly-braced]]))

(def ^:private accent-commands
  {\` "\u0300"
   \' "\u0301"
   \^ "\u0302"
   \" "\u0308"
   \H "\u030B"
   \~ "\u0303"
   \c "\u0327"
   \k "\u0328"
   \= "\u0304"
   \b "\u0331"
   \. "\u0307"
   \d "\u0323"
   \r "\u030A"
   \u "\u0306"
   \v "\u030C"})
(defn accent
  "Reads a TeX accent macro (like \\'a or \\\"{o}) and converts it to a proper
  Unicode string (like á or ö) using a combining character"
  ; TODO: handle multiple accents like \~\i, \~{oo}, \'\i, and \'{\c{c}}
  []
  (let->> [_ (char \\)
           command (token accent-commands)
           body (either (letter) (curly-braced (letter)))]
    ; the combining character goes after the character it modifies
    (always (str body (accent-commands command)))))

(def ^:private fancy-character-commands
  {\l "\u0142"   ; l with stroke: ł
   \o "\u00F8"   ; o with stroke: ø
   \i "\u0131"   ; dotless i: ı
   \j "\u0237"}) ; dotless j: ȷ
(defn fancy-character
  "Read a TeX special character macro (like \\i or \\o) and convert it to an
  equivalent Unicode string (like ı or ø)"
  []
  (let->> [_ (char \\)
           command (token fancy-character-commands)]
    ; the fancy character command might have an empty argument (e.g., \i{})
    ; which will just be appended as an empty string by the next tex-block handler
    (always (fancy-character-commands command))))

(def ^:private tex-delimiters #{\# \$ \% \& \\ \_ \{ \} \~ \@ \space})
(defn escaped-misc
  "Read an escaped TeX character and return the literal string"
  ; TeX's special characters are:
  ;     # $ % & \ ^ _ { } ~
  ; \^ is a valid command, for circumflex accents.
  ; Otherwise, these characters, and a few others that are also special,
  ; can be escaped and pass through directly.
  []
  (let->> [_ (char \\)
           c (token tex-delimiters)]
    (always c)))

(defn macro
  "Strip (i.e., ignore) formatting that cannot be encoded as Unicode"
  ; In the following cases, we want to ignore the macro
  ; and just flatten out the braces as if they were naked blocks
  ;   \emph{Chronik von Deutschland} => Chronik von Deutschland
  ;   {\bf NLP}                      => NLP
  ; it always returns nil, and should come after the fancy-character parser,
  ; which handles a subset of such commands that can be represented in Unicode
  []
  (>> (char \\)
      (many1 (letter))
      (many (token whitespace-chars))
      (always nil)))

; node and block are mutually recursive
(declare node)

; (block) strips the curly braces from a tex-block string and
; recursively handles the contents inside the tex-block as tex
; it returns a string with no TeX commands, escapes, or braces.
(defparser block []
  (let->> [nodes (curly-braced (many (node)))]
    (always (str/join nodes))))

(defn node
  "Return a TeX tree string built by parsing and then generating TeX strings
  in nested parsers, potentially recursively."
  []
  (choice
    (attempt (accent)) ; returns string
    (attempt (fancy-character)) ; returns string
    (attempt (macro)) ; returns nil
    ; discard escaped hyphens (hyphenation hints)
    (attempt (>> (char \\) (char \-) (always nil))) ; returns nil
    (attempt (escaped-misc)) ; returns string
    ; TODO: handle other escaped things?
    (attempt (block)) ; returns seq of strings
    (any-char-except-in #{\\ \{ \}}))) ; anything except slashes or braces

(defn read
  "Parse and simplify the TeX reader into a string of TeX"
  [reader]
  (str/join (run (many (node)) reader)))

(defn read-str
  "Parse and simplify a TeX string into a simplified string of TeX"
  [string]
  (read string))
