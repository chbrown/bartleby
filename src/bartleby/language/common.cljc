(ns bartleby.language.common
  (:refer-clojure :exclude [char])
  (:require [the.parsatron :refer :all])
  (:import [the.parsatron InputState SourcePos]))

(def whitespace-chars #{\space \return \newline \tab \formfeed})
(def whitespace (many (token whitespace-chars)))

(def linebreak (either (>> (char \return) (attempt (char \newline)))
                       (char \newline)))

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

(defn double-quoted [p]
  (between (char \") (char \") p))

(defn before
  "Parse p followed by close, but return the value of p;
   equivalent to (between (always nil) close p)"
  [close p]
  (let->> [x p
           _ close]
    (always x)))

;; stuff that could be in parsatron upstream:

; similar to parsatron/run-parser
(defn run-parser-seq
  [p endp state]
  (letfn [(pcok [item new-state]
            (cons item (lazy-seq (run-parser-seq p endp new-state))))
          (peok [_ {:keys [pos]}]
            (throw (fail (show-error (unexpect-error "that run-seq parser p would accept an empty string" pos)))))
          (perr [err-from-p]
            (letfn [(endpok [_ _]
                      nil)
                    (endperr [err-from-endp]
                      (throw (fail (show-error (merge-errors err-from-p err-from-endp)))))]
              (parsatron-poline endp state endpok endperr endpok endperr)))]
    (parsatron-poline p state pcok perr peok perr)))

; similar to parsatron/run
(defn run-seq
  "Repeatedly run the parser p over the input.
  If the parser produces an error, try endp.
  If endp fails, fail with both p's and endp's errors."
  [p endp input]
  (let [initial-state (InputState. input (SourcePos. 1 1))]
    (run-parser-seq p endp initial-state)))
