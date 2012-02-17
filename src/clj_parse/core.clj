(ns clj-parse.core
  (:use [clojure.test]))

(defn match1 [test-fn]
  (fn [[coll res]]
    (let [x (first coll)]
      (if (test-fn x)
      [(rest coll) (conj res x)]
      nil))))

(defn match1? [test-fn]
  (let [m (match1 test-fn)]
    (fn [ctxt] (or (m ctxt) ctxt))))

(defn match* [test-fn]
  (let [m (match1 test-fn)]
    (fn [ctxt]
      (loop [last-ctxt ctxt
             cur-ctxt (m ctxt)]
        (if cur-ctxt (recur cur-ctxt (m cur-ctxt)) last-ctxt)))))

(defn match+ [test-fn]
  (let [m* (match* test-fn)]
    (fn [ctxt] (let [new-ctxt (m* ctxt)]
                (if (= ctxt new-ctxt) nil new-ctxt)))))

(defn match-transform [test-fn transform]
  (fn [x] (if (test-fn x) (transform x) nil))) 

(defn match-seq [& matchers]
  (fn [ctxt]
    (loop [m (first matchers)
           more (rest matchers)
           last-ctxt ctxt]
      (if m
        (if-let [new-ctxt (m last-ctxt)]
          (recur (first more) (rest more) new-ctxt)
          nil)
        last-ctxt))))

(defn match-or [& matchers]
  (fn [ctxt]
    (loop [m (first matchers)
           more (rest matchers)]
      (when m
        (if-let [new-ctxt (m ctxt)]
          new-ctxt
          (recur (first more) (rest more)))))))