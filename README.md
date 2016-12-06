# bartleby

[![Travis CI Build Status](https://travis-ci.org/chbrown/bartleby.svg)](https://travis-ci.org/chbrown/bartleby)
[![Coveralls Coverage Status](https://coveralls.io/repos/chbrown/bartleby/badge.svg)](https://coveralls.io/github/chbrown/bartleby)

A very _faithful_ BibTeX parser.

- Preserves comments, order, formatting, string interpolations
- _Mostly_ idempotent at its core
  * Whitespace outside values is always standardized
- Implements various levels of cleaning up / standardization beyond whitespace


## References

* Interesting blog post on writing parsers in Clojure with monads at <http://albert.rierol.net/clojure-monads.html>, with code at <https://gist.github.com/acardona/3672948>
  - His answer to whether "whether I could write better clojure programs by using monads"?
    "The short answer is that the disadvantages outweight the advantages."
* "[Kern](https://github.com/blancas/kern) is a library of parser combinators for Clojure"
  - 130 stars on GitHub.
  - It doesn't appear to be compatible with ClojureScript.
* "[The Parsatron](https://github.com/youngnh/parsatron) is a functional parser library"
  - 173 stars on GitHub.
  - Ported to ClojureScript.
  - [doc/guide](https://github.com/youngnh/parsatron/blob/master/doc/guide.markdown)


## Parsatron API

The whole thing is only 316 LOC, and mostly easily readable:
<https://github.com/youngnh/parsatron/blob/master/src/clj/the/parsatron.clj>

> A "parser" is, technically, a function that takes 5 arguments and returns a special value
run is a function that takes a parser and an input, and returns the output (or throws)

The parser 5-tuple:

    state, cok, cerr, eok, eerr

* `state` (`InputState` record) is a 2-tuple of the input, `input`, and the position, `pos`, which is a `SourcePos` (which has two integer fields, `line` and `column`).
* `cok` "I consumed something and I'm still OK"
  - is called to consume valid non-empty input
  - primary example: `(token)`, which succeeds if the input is non-empty, and calls `(cok x next-input-state)`, where `x` is the first thing taken from the input and `next-input-state` is a new `InputState` instance created with the consumed/advanced input and the incremented state tracker. If the input is empty, calls `(eerr new-error)`, where `new-error` conveys a message like "Unexpected end of input".
* `cerr` "I consumed something but I don't know what happened I'm broken"
* `eok` "I didn't consume anything and that's what I expected"
  - is called on when encountering valid empty input
  - it's a function that takes a new token from the input stream, and the current state, and... returns a new state?
  - primary example: `(always x)` which is a parser that consumes nothing and calls `(eok x state)`
* `eerr` "I didn't consume anything and I'm still broken"
  - primary example `(never)`, which consumes nothing and calls `(eerr new-error)`, where `new-error` is something that we can easily check for in the calling container (e.g., in the default implementation, a `ParseError` instance)

The functions `cok`, `cerr`, `eok`, and `eerr` are 'escape hatches' (callbacks, in a way) that the parser uses to exit.
The `*ok` ones (usually) take two arguments: the parsed thing and resulting state.
The `*err` ones take one argument: the Error instance.

I don't know enough about parser combinators to guess why they call a function instead of returning a 2-bit enumeration (e.g., a boolean vector `[consumed?, ok?]`) + the parsed thing or error instance. That would be an inversion of flow, but seems less callback-oriented and thus more straightforward?

I wish the author had explained where `cok` et al. come from in [his talk](https://www.infoq.com/presentations/Parser-Combinators), and I'm surprised no one asked in the extensive Q&A.


#### Built-in parser creators

* `token` takes a single predicate and returns a parser that accepts / rejects based on that predicate and returns the input unchanged
* `char` takes a character and creates a parser that accepts and returns that character only
* `any-char` creates a parser that accepts any character
* `letter` creates a parser that accepts any alphabetic character
* `digit` creates a parser that accepts any numeric character
* `string` takes a string and creates a parser that accepts only that string
* `eof` creates a parser that throws on any input; it only succeeds if there's nothing left, in which case it returns `nil`

#### combiners

* `>>` chains multiple parsers together, in a serial sequence, but it only returns the last parser's result
* `times` (like `{n}`) takes a count (positive integer) and a parser and chains that many of parser together, but returns a sequence
* `many` (like `*`) takes a parser and greedily accepts as much of the input stream as that parser will accept, or nothing (in which case it returns an empty vector)
  - TODO: figure out why it returns a list usually, and a vector for empty results
* `many1` (like `+`) is like `many` but requires at least one match
* `attempt` (like `?`) tries a parser and consumes nothing if it fails
* `lookahead` applies a parser and returns the result but does not consume anything
* `choice` takes multiple parsers and tries each one in turn
* `either` is like `choice` but only takes two parsers
* if `choice` (or `either`) fail to match any (either) of their parsers, it throws an error that is the concatenation of the error messages for each of the parsers, which seems kind of messy but maybe can be configured?
* `between` takes three parsers, `[open close content]`, and runs them like `(>> open content close)`, but returns only the result of the `content` parser.

#### higher level / macro stuff

* `defparser` defines a parser-creator function, which, when called, returns a parser along the lines of `token` or `any-char`
* `let->>` binds a sequence of parsers to their result values
  - > Every right hand side in a `let->>` binding form has to evaluate to a parser.

#### debugging

It's pretty hard to debug a big parsatron grammar.
It'd be madness without the REPL, but building up grammars incrementally with a lot of feedback makes it feasible.

The following error messages are followed by a translation of the (potential) real issue:

    ClassCastException java.lang.String cannot be cast to clojure.lang.IFn
    the.parsatron/bind/fn--204/pcok--205/fn--206/fn--207 (parsatron.clj:97)

You forgot to wrap the string you're trying to return from your `(defparser ...)` in an `(always ...)`.

    ArityException Wrong number of args (5) passed to: PersistentVector
    clojure.lang.AFn.throwArity (AFn.java:429)

You forgot to wrap the vector you're trying to return from your `(defparser ...)` in an `(always ...)`.


## License

Copyright © 2016 Christopher Brown. [MIT Licensed](https://chbrown.github.io/licenses/MIT/#2016).
