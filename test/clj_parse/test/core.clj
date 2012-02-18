(ns clj-parse.test.core
  (:use [clj-parse.core])
  (:use [clojure.test]))

(def MatchExpr (||> [(||> [any (||> [(eq :+)] [(eq :*)] [(eq :?)]) :?]) :+]))

(defn test-match-expr [expr]
  (is (= (MatchExpr expr) expr)))

(deftest test-expressions
  (test-match-expr [keyword? :+])
  (test-match-expr [string? keyword? :+ number? :*]))