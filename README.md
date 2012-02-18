# clj-parse

A library for parsing clojure expressions using a grammar defined
in clojure and optionally transforming expressions at each match.

This software should be considered Alpha quality.  I basically
wrote it because I was learning clojure and saw people doing
lots of parsing within macros.  I wanted to see if it would be
possible to define a language for parsing and transforming
clojure expressions within clojure.  This is what I came up with.

## Usage

There is basically one primary function parser (with an alias ||>)
that can be used to define clojure expression parsers within clojure.
There are also several helper functions and symbols to help with 
creating parsers. Existing parser can be used as match expressions
within parsers.

Here is a grammar of the language understood by the parser function
writen in the syntax of the parser function itself:


### The grammar of the parser expression language:

```clojure
(def expressions
 (||> "Expressions"
  [(||> "Expression
          [(||> "Atom" [any "Test Function or Parser"]    ;; any always returns true
                       [vector? "Vector of Expressions"]) ;; For matching sub-sequences
           string? "Expression Name" :?   
           (||> [(eq :+) "one or many Operator"] ;; (eq x) is equivalent to (partial = x)
                [(eq :*) "zero or many Operator"]
                [(eq :?) "optional Operator"]) "Expression Repeat (default one)" :?
           (||> "Transform Expression" [(eq :=>) "Transform Operator" any "Transform Function"]) :?] :+]))
```

### The grammar of the parser (alias ||>) function itself:

```clojure
  [string? "parser name" [expressions] "grammar expressions" :+ (||> [eq :=> any]) "transform expression" :?]
```
A generated parser will match any one of the [expressions] supplied so
the parser function essentially serves as the or operator as well.

## Example

Here is an example parser:

```clojure
user> (def p1 (||> [keyword? :+ [(||> [keyword? :+] [number? :* :=> #(map (partial + 5) %)])] :? number? :? keyword? :?]))
#'user/p1
user> (p1 [:a [1 2] 5 :b])
[:a [6 7] 5 :b]  ;; Parser adds 5 to the numbers in the matched subsequence
user> (p1 [:a [:c :d] 5 :b])
[:a [:c :d] 5 :b]
user> (p1 [:a 5 :b])
[:a 5 :b]
user> (p1 [:a 5])
[:a 5]
user> (p1 [:a])
[:a]
user> (p1 [1 2 3])
nil ;; Parser returns nil when there is no match
```

Parsers can used to create somewhat unusual macros.  Here
is a silly example:

```clojure
(def crazy-grammar
  (||> [(||> [(||> [(eq 'Say) :=> (constantly #(str % ". "))]
                   [(eq 'Shout) :=> (constantly #(.toUpperCase (str % "! ")))]
                   [(eq 'Whisper) :=> (constantly #(.toLowerCase (str % "... ")))])
               #(and (symbol? %) (not (= '. %))) :+ :=> #(apply str (interpose " " %))
               (||> [number? (eq 'times) :=> ignore]) :? :=> (default 1)
               (eq '.) :=> ignore] :=> vector) :+]))

(defmacro crazy-macro [& words]
  (let [sentences (crazy-grammar words)
        actions (for [[f s n] sentences]
                  `(for [i# (range ~n)] (~f ~s)))]
    `(apply str (flatten [~@actions]))))
```

Trying this macro at out at the repl we can get:

```clojure
user> (crazy-macro Say Hello 3 times . Shout hello world 2 times . Whisper goodbye .)
"Hello. Hello. Hello. HELLO WORLD! HELLO WORLD! goodbye... "
```

## License

Copyright (C) 2012 Aaron Craelius

Distributed under the Eclipse Public License, same as Clojure.
