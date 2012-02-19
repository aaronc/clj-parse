(ns clj-parse.test.core
  (:use [clj-parse.core])
  (:use [clj-parse.helpers])
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

(def crazy-grammar (m+ (mgroup "Action"
                        (mseq (mor "Verb"
                          (mapply (constantly #(str % ". ")) (eq 'Say))
                          (mapply (constantly #(.toUpperCase (str % "! "))) (eq 'Shout))
                          (mapply (constantly #(.toLowerCase (str % "... "))) (eq 'Whisper)))
                          (mapply "Statement" #(apply str (interpose " " %&))
                                  (m+ "Words" #(and (symbol? %) (not (= '. %)))))
                         (mdefault "Repetition" 1 (m? [number? (mignore (eq 'times))]))
                         (mignore (eq '.))))))

(defmacro crazy-macro [& words]
  (let [sentences (crazy-grammar words)
        actions (for [[f s n] sentences]
                  `(for [i# (range ~n)] (~f ~s)))]
    `(apply str (flatten [~@actions]))))

(deftest test-crazy-macro
  (is (= (crazy-macro Say Hello 3 times . Shout hello world 2 times . Whisper goodbye .)
         "Hello. Hello. Hello. HELLO WORLD! HELLO WORLD! goodbye... ")))