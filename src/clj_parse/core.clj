(ns clj-parse.core
  (:use [clojure.test]))

(defprotocol IMatcher (match [this ctxt]))

(def ^:dynamic **debug** false)

(defn- indent [n] (apply str (repeat (dec n) "-")))

(defn- thread-local* [init] ;; From https://github.com/flatland/useful
  (let [generator (proxy [ThreadLocal] []
                    (initialValue [] init))]
        (reify clojure.lang.IDeref
         (deref [this]
         (.get generator)))))

(def log-stack (thread-local* (atom (list))))

(defn- log-top [ret]
  (when **debug**
    (let [top (peek @@log-stack)
          depth (count @@log-stack)
          pre (indent depth)]
      (when (or ret (> depth 0))
        (if ret
          (println (str pre top " => " ret))
          (println (str pre top ":")))))))
        
(defn- push-log [this name]
  (do (log-top nil)
      (let [msg (str "<" (.getSimpleName (class this)) " " (or name "Unknown") ">")]
        (swap! @log-stack conj msg))))

(defn- pop-log [this name ret]
  (do (log-top ret)
      (swap! @log-stack pop)
      ret))

(defn- clear-log-stack [] (swap! @log-stack (fn [_] (list))))

(defmacro logged [this name & body]
  `(try (push-log ~this ~name)
        (let [ret# (do ~@body)]
        (pop-log ~this name ret#)
         ret#)
  (catch Throwable e#
    (clear-log-stack)
    (throw e#))))

(defn- done? [ctxt] (empty? (ctxt 0)))

(defrecord Match1 [name test-fn]
  IMatcher
  (match [this [coll res]]
    (logged this name
        (when-let [x (first coll)]
          (if (test-fn x)
            [(rest coll) (conj res x)]
            nil)))))

(defn match1
  ([name f] (if (satisfies? IMatcher f) f (Match1. name f)))
  ([f] (match1 nil f)))

(defrecord Match? [name matcher] IMatcher (match [this ctxt] (logged this name (or (match matcher ctxt) ctxt))))

(defn- do-match* [matcher ctxt]
    (loop [last-ctxt ctxt]
    (if (done? last-ctxt)
      last-ctxt
      (if-let [cur-ctxt (match matcher last-ctxt)]
                            (if (= cur-ctxt last-ctxt) last-ctxt (recur cur-ctxt))
                            last-ctxt))))

(defrecord Match* [name matcher] IMatcher (match [this ctxt] (logged this name (do-match* matcher ctxt))))

(defrecord Match+ [name matcher]
IMatcher (match [this ctxt]
  (logged this name 
    (let [new-ctxt (do-match* matcher ctxt)]
      (if (= ctxt new-ctxt) nil new-ctxt)))))

(defn- match1?
  ([name f] (Match?. name (match1 f)))
  ([f] (match1? nil f)))

(defn- match* [f] (Match*. nil (match1 f)))

(defn- match+ [f] (Match+. nil (match1 f)))

(defrecord MatchSeq [name matchers]
IMatcher
(match [this ctxt]
  (logged this name
          (loop [m (first matchers)
                 more (rest matchers)
                 last-ctxt ctxt]
            (if m (if-let [new-ctxt (match m last-ctxt)] (recur (first more) (rest more) new-ctxt) nil)
                last-ctxt)))))

(defrecord MatchOr [name matchers]
IMatcher
(match [this ctxt]
  (logged this name
          (loop [m (first matchers)
                 more (rest matchers)]
            (when m
              (if-let [new-ctxt (match m ctxt)]
                new-ctxt
                (recur (first more) (rest more))))))))

(defn- matches-entirely? [[coll res]]
  (when (empty? coll) res))

(defn- do-match-sub-seq [matcher ctxt]
 (let [[coll res] ctxt
      x (first coll)
      more (rest coll)]
  (log "do-match-sub-seq : " matcher " @ " x)
  (when (sequential? x)
    (when-let [sub-ctxt (match matcher [x []])]
      (when (done? sub-ctxt) [more (conj res (second sub-ctxt))])))))

(defrecord MatchSubSeq [name matcher]
  IMatcher (match [this ctxt] (logged this name (do-match-sub-seq matcher ctxt))))

(defrecord MatchTransformer [name matcher transform]
IMatcher
(match [this ctxt]
  (logged this name
          (when-let [new-ctxt (match matcher ctxt)]
            (let [orig-res (second ctxt)
                  start (count orig-res)
                  new-res (second new-ctxt)
                  end (count new-res)
                  transformed (transform (subvec new-res start end))]
              [(first new-ctxt)
               (if transformed
                 (if (sequential? transformed) (into orig-res transformed) (conj orig-res transformed))
                 orig-res)])))))

;; Convenience mini-language definitions
(def any (constantly true))

(def ignore (constantly nil))

(defn eq [x] (fn [y] (= x y)))

(defn default [value] (fn [res] (if (empty? res) value res)))

(defn unary-op [from to] (MatchTransformer. (str "TransformMatchOperator " from) (match1 (eq from)) (constantly to)))

(declare match-expr)

(defrecord SubSeqMatchExpr []
  IMatcher
  (match [this ctxt] (logged "SubSeqMatchExpr" (do-match-sub-seq match-expr ctxt))))

(def match-sub-seq-expr (MatchTransformer. "TransformSubSeqMatchExpr" (SubSeqMatchExpr.) (fn [[matchers]] (MatchSubSeq. nil (MatchSeq. nil matchers)))))

(def match-atom
  (MatchSeq. "MatchAtomWithName"
    [(MatchOr. "MatchAtom" [match-sub-seq-expr (match1 any)])
     (MatchTransformer. "DefaultMatchName" (match1? "MatchName" string?) (default [nil]))]))

(def match-occurrence-expr
  (MatchTransformer. "TransformMatchOccurrenceExpr"
    (MatchSeq. "MatchOccurrenceExpr"
      [match-atom
       (MatchTransformer. "TransformMatchOperator?"
         (Match?. "MatchOperator?"
           (MatchOr. "MatchOperators"
             [(unary-op :+ match+) (unary-op :* match*) (unary-op :? match1?)]))
         (default match1))])
    (fn [[expr name modifier]] (assoc (modifier expr) :name name))))

(def match-transform-expr
  (MatchTransformer. "TransformMatchTransformExpr"
    (MatchSeq. "MatchTransformExpr" [match-occurrence-expr (MatchTransformer. "MatchTransformOperator" (match1 (eq :=>)) ignore) (match1 any)])
    (fn [[matcher transform]] (MatchTransformer. nil matcher transform))))

(def primary-match-expr (MatchOr. "PrimaryMatchExpr" [match-transform-expr match-occurrence-expr]))

(def match-expr (Match+. "MatchExpr" primary-match-expr))

(defn matcher
([forms] (MatchSeq. "Matcher" (matches-entirely? (match match-expr [forms []]))))
([forms _ transform] (MatchTransformer. nil (matcher forms) transform)))

(defrecord Parser [matcher]
IMatcher (match [this ctxt] (match matcher ctxt))
clojure.lang.IFn (invoke [this coll] (second (match this [coll []]))))

(defn >>>
  "Creates a parser for parsing and transforming Clojure symbols
using a grammar defined within Clojure."
  ([forms] (Parser. (matcher forms)))
  ([forms _ transform] (Parser. (matcher forms _ transform))))

(defn || [& forms] (Parser. (MatchOr. "MatchOr" (map #(if (satisfies? IMatcher %) % (matcher %)) forms))))

(def parser-expr
 (Parser.
  (MatchTransformer. "ParserGen"
    (MatchSeq. "ParserExpr"
      [(MatchTransformer. "DefaultParseName" (match1? "ParserName" string?) (default [nil]))
       (MatchTransformer. "GroupParseExpressions"
         (Match+. "ParseExpressions"
           (MatchOr. "ParseExpression"
             [(match1 "MatcherRef" (partial satisfies? IMatcher))
              (MatchTransformer. "MatchVectorSeq"
                (MatchSubSeq. "MatchVector" match-expr)
                (fn [matchers] (MatchSeq. nil (do (assert (= 1 (count matchers))) (first matchers)))))]))
         (fn [parsers] [parsers]))
       (MatchTransformer. "DefaultParserTransform"
         (match1? "ParserTransform?"
           (MatchSeq. "ParserTransformSeq"
             [(MatchTransformer. "Ignore :=>" (match1 "TransformOperator" (eq :=>)) ignore)
              (match1 "TransformExpression" any)]))
         (default [nil]))])
    (fn [[name parsers transform]]
        (let [parser
              (if (= 1 (count parsers))
                (assoc (first parsers) :name name)
                (MatchOr. name parsers))]
          (Parser.
           (if transform
             (MatchTransformer. nil parser transform)
             parser)))))))

(defn parser [& forms] (first (parser-expr forms)))
(def ||> parser)