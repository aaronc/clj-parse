(ns clj-parse.test.core
  (:use [clj-parse.core])
  (:use [clojure.test]))


(deftest test-match1
  (let [m (partial match (m1 keyword?))]
    (is (= (m [[:test] []]) [[] [:test]]))
    (is (= (m [[5] []]) nil))))

(deftest test-match1?
  (let [m (partial match (m? keyword?))]
    (is (= (m [[:test] []]) [[] [:test]]))
    (is (= (m [[5] []]) [[5] []]))))

(deftest test-match*
  (let [m (partial match (m* keyword?))]
    (is (= (m [[:a :b :c 4 5 6] []])
           [[4 5 6] [:a :b :c]]))
    (is (= (m [[1 2] []]) [[1 2] []]))))

(deftest test-match+
  (let [m (partial match (m+ keyword?))]
    (is (= (m [[:a :b :c 4 5 6] []])
           [[4 5 6] [:a :b :c]]))
    (is (= (m [[1 2] []]) nil))))

(deftest test-match-seq
  (let [m (partial match (mseq (m* keyword?) (m1 symbol?) (m? symbol?) (m+ number?)))]
    (is (= (m [[:a :b :c 'x 1 2 3] []]) [[] [:a :b :c 'x 1 2 3]]))
    (is (= (m [[:a :b :c 'x 'y 1 2 3] []]) [[] [:a :b :c 'x 'y 1 2 3]]))
    (is (= (m [[:a :b :c 'x 'y 'z 1 2 3] []]) nil))))

(deftest test-match-or
  (let [m (partial match (mor (m+ keyword?) (m1 symbol?) (m+ number?)))]
    (is (= (m [[:a :b :c] []]) [[] [:a :b :c]]))
    (is (= (m [['x 1] []]) [[1] ['x]]))
    (is (= (m [[1 2 3] []]) [[] [1 2 3]]))
    (is (= (m [["xyx"] []]) nil))))

(deftest test-match-apply-subseq-parse
  (let [p (partial parse (mseq (mor keyword? number?)
                               (mapply first (msubseq (mapply + (m* number?))))))]
    (is (= (p [1 [1 2 3]]) [1 6]))))

