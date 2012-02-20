# clj-parse

A simple framework for creating parsers that parse
(and possibly transform) sequences of clojure symbols
into sequences of clojure symbols.  This could be useful
for generating macros, functions with complex syntax, or
even parsing test.

The primarly design goal is to make it easy to generate correct
parsers, but performance is not a primary consideration.
Scenarios that do not depend on speed, such as generating macros,
are good use cases.  The entire framework is expressed in about
200 lines of code so that it can be easily embedded in projects
that do not want an extra dependency.

This library should be considered **alpha** quality as it has
not been used in production.

## Examples

Here are some examples of parsers that can be generated:

```clojure
user> (use 'clj-parse.core)
nil
user> (def parser1 (mseq keyword? (mapply * (m+ number?))))
#'user/parser1
user> (parser1 [:x 1 2 3 4 5])
[:x 120]
user> (def parser2 (mapply hash-map
           (m+ (mseq keyword? (mapply * (m+ number?))))))
#'user/parser2
user> (parser2 [:a 1 2 3 :b 4 5 6 :c 10 20 30])
[{:a 6, :c 6000, :b 120}]
user> (def parser3 (mapply hash-map
           (m+ (mseq keyword?
               (mor (mapply str (m+ string?)) (mapply * (m+ number?)))))))
#'user/parser3
user> (parser3 [:a 1 2 3 :b "H" "e" "ll" "o" :c 10 20 30])
[{:a 6, :c 6000, :b "Hello"}]
```

### Alternate Syntax (Experimental)

There is also an alternate syntax which is intended to imitate EBNF notation (currently in the
clj-parse.pseudoebnf namespace).  Using that syntax, the above examples would be written as:

```clojure
user> (use 'clj-parse.pseudoebnf)
nil
user> (def parser1 (grammar keyword? number? :+ :=> *))
#'user/parser1
user> (parser1 [:x 1 2 3 4 5])
[:x 120]
user> (def parser2 (grammar [keyword? number? :+ :=> *] :+ :=> hash-map))
#'user/parser2
user> (parser2 [:a 1 2 3 :b 4 5 6 :c 10 20 30])
[{:a 6, :c 6000, :b 120}]
user> (def parser3 (grammar [keyword? (|| [number? :+ :=> *] [string? :+ :=> str])] :+ :=> hash-map))
#'user/parser3
user> (parser3 [:a 1 2 3 :b "H" "e" "ll" "o" :c 10 20 30])
[{:a 6, :c 6000, :b "Hello"}]
```
(Note that the generated parsers should be equivalent in both internal structure and usage.)

## Usage

Please see the [generated documentation](http://aaronc.github.com/clj-parse/) for details on usage.
You can see some interesting examples in the [tests](http://aaronc.github.com/clj-parse/grammar-examples.clj). A macro `dbg-parser` is available for debugging
parsers and all matcher functions take an optional string as their first parameter which shows up in
debugging output.

## License

Copyright (C) 2012 Aaron Craelius

Distributed under the Eclipse Public License, the same as Clojure.
