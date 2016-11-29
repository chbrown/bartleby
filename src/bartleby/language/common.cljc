(ns bartleby.language.common
  (:refer-clojure :exclude [char])
  (:require [the.parsatron :refer :all]))

(def whitespace-chars #{\space \newline \tab})
(def whitespace (many (token whitespace-chars)))

(defn maybe [p]
  ; not sure if the "attempt" is necessary, but it seems like attempt really ought
  ; to be the default in parsatron: https://github.com/youngnh/parsatron/issues/15
  ; the README reads: "offers infinite lookahead", but fails to mention that that
  ; offer is only valid when wrapped in an (attempt ...)
  (either (attempt p) (always nil)))

(defn any-char-except-in [x]
  (token #(and (char? %) (not (contains? x %)))))

(defn any-char-except [x]
  (token #(and (char? %) (not= % x))))

(defn curly-braced [p]
  ; p, e.g., (many (any-char-except #{\{ \}}))
  (between (char \{) (char \}) p))

(defn before
  "Parse p followed by close, but return the value of p;
   equivalent to (between (always nil) close p)"
  [close p]
  (let->> [x p
           _ close]
    (always x)))
