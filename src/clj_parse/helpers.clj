(ns clj-parse.helpers
  (:use [clj-parse.core]))

(def any? (constantly true))

(defn eq [x] (fn [y] (= x y)))

(defn default [value] (fn [& res] (if (empty? res) value res)))

(def mdefault (make-match-transform-fn (fn [name t m] (mapply (str name "*default") (default t) m))))

(def ignore (constantly nil))

(def mignore (make-match1-fn (fn [name m] (mapply (str name "*ignore") ignore m))))

(defn group [& matches] [matches])

(def mgroup (make-match1-fn (fn [name m] (mapply (str name "*group") group m))))