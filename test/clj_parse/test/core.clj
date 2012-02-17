(ns clj-parse.test.core
  (:use [clj-parse.core])
  (:use [clojure.test]))

(deftest test-match1
  (let [m (match1 keyword?)]
    (is (= (m [[:test] []]) [[] [:test]]))
    (is (= (m [[5] []]) nil))))

(deftest test-match1?
  (let [m (match1? keyword?)]
    (is (= (m [[:test] []]) [[] [:test]]))
    (is (= (m [[5] []]) [[5] []]))))

(deftest test-match*
  (let [m (match* keyword?)]
    (is (= (m [[:a :b :c 4 5 6] []])
           [[4 5 6] [:a :b :c]]))
    (is (= (m [[1 2] []]) [[1 2] []]))))

(deftest test-match+
  (let [m (match+ keyword?)]
    (is (= (m [[:a :b :c 4 5 6] []])
           [[4 5 6] [:a :b :c]]))
    (is (= (m [[1 2] []]) nil))))

(deftest test-match-seq
  (let [m (match-seq (match* keyword?) (match1 symbol?) (match1? symbol?) (match+ number?))]
    (is (= (m [[:a :b :c 'x 1 2 3] []]) [[] [:a :b :c 'x 1 2 3]]))
    (is (= (m [[:a :b :c 'x 'y 1 2 3] []]) [[] [:a :b :c 'x 'y 1 2 3]]))
    (is (= (m [[:a :b :c 'x 'y 'z 1 2 3] []]) nil))))

(deftest test-match-or
  (let [m (match-or (match+ keyword?) (match1 symbol?) (match+ number?))]
    (is (= (m [[:a :b :c] []]) [[] [:a :b :c]]))
    (is (= (m [['x 1] []]) [[1] ['x]]))
    (is (= (m [[1 2 3] []]) [[] [1 2 3]]))
    (is (= (m [["xyx"] []]) nil))))


(comment (deftest test-match-expr
           (let [test-exp (fn [e] (is (= (match-match-expr e) e)))]  
             (test-exp [keyword? symbol? '?]) 
             (test-exp '[|| (keyword? symbol? ?)
                         (number? symbol? number? (|| (number? ?) (string? ?)))]))))