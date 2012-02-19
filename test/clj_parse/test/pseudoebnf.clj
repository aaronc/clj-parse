(ns clj-parse.test.pseudoebnf
  (:use [clj-parse.pseudoebnf])
  (:use [clj-parse.helpers])
  (:use [clojure.test]))

(def SimpleMatchExpr (>>> (>>> any? (|| (eq :+) (eq :*) (eq :?)) :?) :+))

(defn test-match-expr [expr]
  (is (= (SimpleMatchExpr expr) expr)))

(deftest test-expressions
  (test-match-expr [keyword? :+])
  (test-match-expr [string? keyword? :+ number? :*]))

(comment
  (def crazy-grammar2
    (||> [(||> [(||> [(eq 'Say) :=> (constantly #(str % ". "))]
                     [(eq 'Shout) :=> (constantly #(.toUpperCase (str % "! ")))]
                     [(eq 'Whisper) :=> (constantly #(.toLowerCase (str % "... ")))])
                #(and (symbol? %) (not (= '. %))) :+ :=> #(apply str (interpose " " %))
                (||> [number? (eq 'times) :=> ignore]) :? :=> (default 1)
                (eq '.) :=> ignore] :=> vector) :+]))

  (defmacro crazy-macro2 [& words]
    (let [sentences (crazy-grammar2 words)
          actions (for [[f s n] sentences]
                    `(for [i# (range ~n)] (~f ~s)))]
      `(apply str (flatten [~@actions]))))

  (deftest test-crazy-macro2
    (is (= (crazy-macro2 Say Hello 3 times . Shout hello world 2 times . Whisper goodbye .)
           "Hello. Hello. Hello. HELLO WORLD! HELLO WORLD! goodbye... "))))

