# clj-parse

A library for parsing clojure expressions using a grammar defined
in clojure and optionally transforming expressions at each match.

This software should be considered Alpha quality.  I basically
wrote it because I was learning clojure and saw people doing
lots of parsing within macros.  I wanted to see if it would be
possible to define a language for parsing and transforming
clojure expressions within clojure.  This is what I came up with.

## Usage

There is basically one primary function parser that can be used to
define clojure expression parsers within clojure.  There are also
several helper functions and symbols to help with creating parsers.
Existing parser can be used as match expressions within parsers.

Here is a grammar of the language understood by the parser function
writen in the syntax of the parser function itself:

```clojure
;; A match-atom can be a sub-sequence parser or any matcher function
;; taking one argument and returning bool if the argument is a valid match.
;; match-sub-seq-expr is a parser that is defined to match a sub-sequence of
;; parsable symbols.  any is a convenience function that always returns true,
;; || is a convenience function which creates a parser which matches greedily
;; any one of the subsequent sub-grammars. 
(def match-atom (|| [match-sub-seq-expr] [any])

;; eq is the following convenience function  (defn eq [x] (fn [y] (= x y))
;; +, *, ? are clojure.core/+, clojure.core/*, and clj-parse.core/?
;; these symbols are used directly in the grammar for simplicity
(def match-occurrence-expr (parser [match-atom (|| [(eq +) (eq *) (eq ?))]))

;; >>> is an alias for parser
;; any in this case represents a transform function taking a sequence
;; of parsed expression from the previous match expression and returning
;; a single value or sequence of values that will be appended to the parse
;; output, or nil to ignore the symbol.  By default matching expressions
;; are appended to the parse output without modification.  
(def match-transform-expr (>>> [match-occurrence-expr (eq =>) any]
  
;; A primary-match-expr is something like:
;;   keyword? +                  ; simply appends keywords to the output sequence
;;      or
;;   keyword? + => #(map name %) ; transforms keywords into strings
(def primary-match-expr (|| match-transform-expr match-occurrence-expr))

;; A match-expr is a sequence of primary-match-expr's
(def match-expr (>>> primary-match-expr +)
```

## Example

Here is an example parser:

```clojure
user> (def my-parser (>>> [keyword? + => #(apply str %) [[number? *]] ? any ?]))
#'user/my-parser
user> (my-parser [:a :b :c [1 2 3]])
[":a:b:c" [1 2 3]]
user> (my-parser [:a "a"])
[":a" "a"]
```

## License

Copyright (C) 2012 Aaron Craelius

Distributed under the Eclipse Public License, same as Clojure.
