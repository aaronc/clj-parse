(ns clj-parse.core
  (:use [clojure.test]))

(defprotocol IMatcher (match [this ctxt]))

(def ^:dynamic **debug** false)

(defn- log [& forms] (when **debug** (println (apply str forms))))

(defn- done? [ctxt] (empty? (ctxt 0)))

(defrecord Match1 [test-fn]
  IMatcher
  (match [this [coll res]]
    (when-let [x (first coll)]
      (if (test-fn x)
      [(rest coll) (conj res x)]
      nil))))

(defn match1 [f] (if (satisfies? IMatcher f) f (Match1. f)))

(defrecord Match? [matcher] IMatcher (match [this ctxt] (or (match matcher ctxt) ctxt)))

(defn match1? [f] (Match?. (match1 f)))

(defn- do-match* [matcher ctxt]
  (loop [last-ctxt ctxt]
    (log "do-match* : " matcher " @ " last-ctxt)
    (if (done? last-ctxt)
      last-ctxt
      (if-let [cur-ctxt (match matcher last-ctxt)]
                            (if (= cur-ctxt last-ctxt) last-ctxt (recur cur-ctxt))
                            last-ctxt))))

(defrecord Match* [matcher] IMatcher (match [this ctxt] (do-match* matcher ctxt)))
  
(defn match* [f] (Match*. (match1 f)))

(defrecord Match+ [matcher]
  IMatcher (match [this ctxt]
   (let [new-ctxt (do-match* matcher ctxt)]
     (if (= ctxt new-ctxt) nil new-ctxt))))

(defn match+ [f] (Match+. (match1 f)))

(defrecord MatchSeq [matchers]
  IMatcher
  (match [this ctxt]
    (loop [m (first matchers)
           more (rest matchers)
           last-ctxt ctxt]
      (log this " : " m " @ " last-ctxt)
      (if m (if-let [new-ctxt (match m last-ctxt)] (recur (first more) (rest more) new-ctxt) nil)
          last-ctxt))))

(defn match-seq [matchers] (MatchSeq. matchers))

(defrecord MatchOr [matchers]
  IMatcher
  (match [this ctxt]
    (loop [m (first matchers)
           more (rest matchers)]
      (log this " : " m)
      (when m
        (if-let [new-ctxt (match m ctxt)]
          new-ctxt
          (recur (first more) (rest more)))))))

(defn match-or [matchers] (MatchOr. matchers))

(defn matches-entirely? [[coll res]]
  (when (empty? coll) res))

(defn- do-match-sub-seq [matcher ctxt]
  (let [[coll res] ctxt
          x (first coll)
          more (rest coll)]
      (when (seq? x)
        (when-let [[coll2 res2] (match matcher [x []])]
          (when (empty? coll2) [more (conj res res2)])))))

(defrecord MatchSubSeq [matcher]
  IMatcher (match [this ctxt] (do-match-sub-seq matcher ctxt)))

(defn match-sub-seq [matcher] (MatchSubSeq. matcher))

(defrecord MatchTransformer [matcher transform]
  IMatcher
  (match [this ctxt]
    (when-let [new-ctxt (match matcher ctxt)]
      (let [orig-res (second ctxt)
            start (count orig-res)
            new-res (second new-ctxt)
            end (count new-res)
            transformed (transform (subvec new-res start end))]
        [(first new-ctxt)
         (if transformed
           (if (sequential? transformed) (into orig-res transformed) (conj orig-res transformed))
           orig-res)]))))

(defn match-transform [matcher transform] (MatchTransformer. matcher transform))

;; Convenience mini-language definitions
(def any (constantly true))

(def ignore (constantly nil))

(def ?)

(def =>)

(defn eq [x] (fn [y] (= x y)))

(defn default [value] (fn [res] (if (empty? res) value res)))

(defn unary-op [from to] (MatchTransformer. (match1 (eq from)) (constantly to)))

(def match-occurrence-expr
  (MatchTransformer.
   (MatchSeq. [(match1 any)
               (MatchTransformer.
                (Match?. (MatchOr. [(unary-op + match+) (unary-op * match*) (unary-op ? match1?)]))
                (default match1))])
   (fn [[expr modifier]] (modifier expr))))

(def match-transform-expr
  (MatchTransformer.
   (MatchSeq. [match-occurrence-expr (MatchTransformer. (match1 (eq =>)) ignore) (match1 any)])
   (fn [[matcher transform]] (MatchTransformer. matcher transform))))

(declare match-expr)

(defrecord SubSeqMatchExpr []
  IMatcher
  (match [this ctxt] (do-match-sub-seq match-expr ctxt)))

(def match-sub-seq-expr (MatchTransformer. (SubSeqMatchExpr.) (fn [[m]] (MatchSubSeq. m))))

(def primary-match-expr (MatchOr. [match-sub-seq-expr match-transform-expr match-occurrence-expr]))

(def match-expr (Match+. primary-match-expr))

(defn matcher
  ([forms] (MatchSeq. (matches-entirely? (match match-expr [forms []]))))
  ([forms _ transform] (MatchTransformer. (matcher forms) transform)))

(defrecord Parser [matcher]
  IMatcher (match [this ctxt] (match matcher ctxt))
  clojure.lang.IFn (invoke [this coll] (second (match this [coll []]))))

(defn parser
  ([forms] (Parser. (matcher forms)))
  ([forms _ transform] (Parser. (matcher forms _ transform))))

(defn || [& forms] (MatchOr. (map #(if (satisfies? IMatcher %) % (matcher %)) forms)))