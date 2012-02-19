# clj-parse

A simple framework for creating parsers that parse
(and possibly transform) sequences of clojure symbols
into sequences of clojure symbols.  This could be useful
for generating macros, functions with complex syntax, or
even parsing text if the text has been tokenized into clojure
symbols.

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
Please see the [generated documentation](http://aaronc.github.com/clj-parse/) for details.

## License

Copyright (C) 2012 Aaron Craelius

Distributed under the Eclipse Public License, the same as Clojure.
