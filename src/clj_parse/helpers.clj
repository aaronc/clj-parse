(ns clj-parse.helpers
  (:use [clj-parse.core]))

;; Match test functions
(defn mexcept [& exclusions]
  (let [exclset (set exclusions)]
     (m1 (str "anything except one of ["
           (interpose "," exclusions) "]")
         #(not (exclset %)))))

(defn default [value] (fn [& res] (if (empty? res) value res)))

(def mdefault (make-match-transform-fn (fn [name t m] (mapply (str name "*default") (default t) m))))

(def ignore (constantly nil))

(def mignore (make-match1-fn (fn [name m] (mapply (str name "*ignore") ignore m))))

(defn group [& matches] [matches])

(def mgroup (make-match1-fn (fn [name m] (mapply (str name "*group") group m))))

(def mconstantly (make-match-transform-fn (fn [name t m] (mapply (str name "*constantly") (constantly t) m))))

(defn msubseq
  "Creates a matcher which if it encounters a sequence as the first token,
  will try to match the sequence of matchers against the tokens in the sub-sequence, failing
  if the matchers do not parse the subsequence tokens completely, and
  appending the array of results to the result sequence as an array."
  ([name sub-parser] (msub (or name "sub-sequence") (fn [x] (when (sequential? x) (sub-parser x)))))
  ([sub-parser] (msubseq nil sub-parser)))