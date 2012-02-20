(ns clj-parse.pseudoebnf
  (:use [clj-parse.core])
  (:use [clj-parse.helpers]))

(declare grammar)

(def GrammarExpr
  (mapply mseq (m+ "match expressions"
       (mapply
        (fn [matcher name op transform]
          (let [m* (op name matcher)] (if transform (mapply transform m*) m*))) 
        (mseq "match espression"
              (mor (msubseq "nested sequence expression" #(apply grammar %))
                   #(and (not (sequential? %)) (not (#{:* :+ :?} %))))
              (mdefault [nil] (m? "matcher name" string?))
              (mdefault m1
                        (m? (mor "match operator"
                                 (mconstantly m* :*)
                                 (mconstantly m+ :+)
                                 (mconstantly m? :?))))
              (mdefault [nil] (m? (mseq
                                   "match transform expression" (mignore :=>)
                                   (m1 "transform function" ifn?)))))))))

(defn grammar [& forms] (first (parse-ex GrammarExpr forms)))

(def GrammarOrExpr
  (mapply
   (fn [name matcher] (if name (assoc matcher :name name) matcher))
   (mseq
       (mdefault [nil] (m? string?))
       (mapply mor (m+ (mor (msubseq "grammar sequence" grammar)
                        (mapply m1 (m1 "matcher expression" ifn?))))))))

(defn || [& forms] (first (GrammarOrExpr forms)))
