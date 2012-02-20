(ns clj-parse.test.pseudoebnf
  (:use [clj-parse.pseudoebnf])
  (:use [clj-parse.helpers])
  (:use [clojure.test]))

(def SimpleMatchExpr (grammar [(mexcept :+ :* :?) (|| :+ :* :?) :?] :+))

(defn test-match-expr [expr]
  (is (= (SimpleMatchExpr expr) expr)))

(deftest test-expressions
  (test-match-expr [keyword? :+])
  (test-match-expr [string? keyword? :+ number? :*]))

(def crazy-grammar
  (grammar [[(||  ['Say :=> (constantly #(str % ". "))]
                  ['Shout :=> (constantly #(.toUpperCase (str % "! ")))]
                  ['Whisper :=> (constantly #(.toLowerCase (str % "... ")))])
                 #(and (symbol? %) (not= '. %)) :+ :=> #(apply str (interpose " " %&))
                 [number? 'times :=> ignore] :? :=> (default 1)
                 '. :=> ignore] :=> group] :+))

(defmacro crazy-macro [& words]
  (let [sentences (crazy-grammar words)
        actions (for [[f s n] sentences]
                  `(for [i# (range ~n)] (~f ~s)))]
    `(apply str (flatten [~@actions]))))

(deftest test-crazy-macro
  (is (= (crazy-macro Say Hello 3 times . Shout hello world 2 times . Whisper goodbye .)
         "Hello. Hello. Hello. HELLO WORLD! HELLO WORLD! goodbye... ")))

