(ns bartleby.language.tex
  (:refer-clojure :exclude [char read])
  (:require [the.parsatron :refer :all]
            [bartleby.language.common :refer :all]))

(def ^:private accent-commands {
  \` "\u0300"
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
  \v "\u030C"
})
; captures accent escapes like:
;   \'a   => á
;   \"{o} => ö
; But not:
;   \~{oo}
;   \~\i
; TODO: handle stuff like \'\i and composed modifiers like \'{\c{c}}
; returns the proper Unicode string
(defparser accent []
  (let->> [_ (char \\)
           command (token accent-commands)
           body (either (letter) (curly-braced (letter)))]
    ; the accent modifier goes after the character it modifies
    (always (str body (accent-commands command)))))

(def ^:private fancy-character-commands {
  \l "\u0142" ; l with stroke: ł
  \o "\u00F8" ; o with stroke: ø
  \i "\u0131" ; dotless i: ı
  \j "\u0237" ; dotless j: ȷ
})
; captures fancy characters like:
;   \i => ı
;   \o => ø
; returns a Unicode string
(defparser fancy-character []
  (let->> [_ (char \\)
           command (token fancy-character-commands)]
    ; the fancy character command might have an empty argument (e.g., \i{})
    ; which will just be appended as an empty string by the next tex-block handler
    (always (fancy-character-commands command))))

; TeX's special characters are:
;     # $ % & \ ^ _ { } ~
; \^ is a valid command, for circumflex accents.
; Otherwise, these characters, and a few others that are also special,
; can be escaped and pass through directly.
(def ^:private tex-delimiters #{\# \$ \% \& \\ \_ \{ \} \~ \@ \space})
(defparser escaped-misc []
  (let->> [_ (char \\)
           c (token tex-delimiters)]
    (always c)))

; (macro) strips (i.e., ignores) formatting that cannot be encoded as Unicode.
; For example, in the following cases, we want to ignore the macro
; and just flatten out the braces as if they were naked blocks
;   \emph{Chronik von Deutschland} => Chronik von Deutschland
;   {\bf NLP}                      => NLP
; it always returns nil, and should come after the fancy-character parser,
; which handles a subset of such commands that can be represented in Unicode
(defparser macro []
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
  (let->> [block-nodes (curly-braced (many (node)))]
    (always (apply str block-nodes))))

; (node) returns a node from a TeX tree, which are all (potentially recursively)
; converted to strings by nested parsers
(defparser node []
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
  "Take a string of TeX, return a string of TeX"
  [reader]
  (run (block) reader))

(defn read-str [s] (read s))
