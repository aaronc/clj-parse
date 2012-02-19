(ns clj-parse.pseudoebnf
  (:use [clj-parse.core])
  (:use [clj-parse.helpers]))

(def MatchExpr
  (mapply mseq (m+ "MatchExpressions"
       (mapply
        (fn [matcher name op transform]
          (let [m* (op name matcher)] (if transform (mapply transform m*) m*))) 
        (mseq "MatchExpression"
              (mapply #(if (sequential? %) (apply msubseq (MatchExpr %)) %) (m1 "Matcher" identity))
              (mdefault [nil] (m? "MatchName" string?))
              (mdefault m1
                        (m? (mor "Match Operators"
                                 (mconstantly m* (eq :*))
                                 (mconstantly m+ (eq :+))
                                 (mconstantly m? (eq :?)))))
              (mdefault [nil] (m? (mseq "MatchTransformExpr" (mignore (eq :=>)) identity))))))))

(defn >>> [& forms] (first (MatchExpr forms)))

(def MatchOrExpr
  (mapply
   (fn [name matcher] (if name (assoc matcher :name name) matcher))
   (mseq
       (mdefault [nil] (m? string?))
       (mapply mor (m+ "OrExpression" (mapply #(if (satisfies? IMatcher %) %
                                                   (if (sequential? %) (apply >>> %)
                                                       (m1 %))) identity))))))

(defn || [& forms] (first (MatchOrExpr forms)))
