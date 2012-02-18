(ns clj-parse.test.core
  (:use [clj-parse.core])
  (:use [clojure.test]))

(def MatchExpr (||> [(||> [any (||> [(eq :+)] [(eq :*)] [(eq :?)]) :?]) :+]))

(defn test-match-expr [expr]
  (is (= (MatchExpr expr) expr)))

(deftest test-expressions
  (test-match-expr [keyword? :+])
  (test-match-expr [string? keyword? :+ number? :*]))

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

(deftest test-crazy-macro
  (is (= (crazy-macro Say Hello 3 times . Shout hello world 2 times . Whisper goodbye .)
         "Hello. Hello. Hello. HELLO WORLD! HELLO WORLD! goodbye... ")))

