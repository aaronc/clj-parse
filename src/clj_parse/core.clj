(ns clj-parse.core)

(defprotocol IMatcher (match [this ctxt]))

(def ^:dynamic **debug** nil)

(def ^:private ^:dynamic log-stack (atom (list)))

(defn- log-top [action ctxt]
  (do (when **debug**
        (let [top (peek @log-stack)
              depth (count @log-stack)
              pre (apply str (repeat (dec depth) "-"))]
          (when (> depth 0) (**debug** (str pre top action ctxt)))))
      ctxt))
        
(defn- push-log [this name ctxt]
  (do (log-top " passed " ctxt)
      (let [msg (str "<" (.getSimpleName (class this)) " "
                     (or name "Unknown") ">")]
        (swap! log-stack conj msg))))

(defn- pop-log [ret] (do (log-top " returned " ret) (swap! log-stack pop)))

(defn- clear-log-stack [] (swap! log-stack (fn [_] (list))))

(defn parse [parser forms] (let [[coll res] (match parser [forms []])]
                             (when (empty? coll) res)))

(defmacro dbg-parser [& body]
  `(binding [**debug** println log-stack (atom (list))] ~@body))

(defmacro dbg-parser* [debug-redirect & body]
  `(binding [**debug** ~debug-redirect log-stack (atom (list))] ~@body))

(defmacro defparsertype
  [name [mname & args] [this ctxt] & body]
      `(defrecord ~name [~mname ~@args]
         IMatcher
         (match [~this ~ctxt] 
           (try (when clj-parse.core/**debug** (clj-parse.core/push-log ~this ~mname ~ctxt))
                      (let [ret# (do ~@body)]
                        (when clj-parse.core/**debug** (clj-parse.core/pop-log ret#))
                        ret#)
                      (catch Throwable e#
                        (when clj-parse.core/**debug**
                          (clj-parse.core/log-top " received exception " e#)
                          (clj-parse.core/clear-log-stack))
                        (throw e#))))
           clojure.lang.IFn
           (clojure.lang.IFn/invoke [this# forms#] (clj-parse.core/parse this# forms#))))

(defn- done? [ctxt] (empty? (ctxt 0)))

(defparsertype Match1 [name test-fn]
  [this [coll res]] (when-let [x (first coll)]
          (if (test-fn x)
            [(rest coll) (conj res x)]
            nil)))

(declare mseq)

(defn m1
  ([name f]
     (if (satisfies? IMatcher f) f
       (if (sequential? f)
         (apply mseq f)
         (Match1. name f))))
  ([f] (m1 nil f)))

(defparsertype Match? [name matcher] [this ctxt] (or (match matcher ctxt) ctxt))

(defn- do-match* [matcher ctxt]
    (loop [last-ctxt ctxt]
    (if (done? last-ctxt)
      last-ctxt
      (if-let [cur-ctxt (match matcher last-ctxt)]
                            (if (= cur-ctxt last-ctxt) last-ctxt (recur cur-ctxt))
                            last-ctxt))))

(defparsertype Match*  [name matcher] [this ctxt] (do-match* matcher ctxt))

(defparsertype Match+  [name matcher] [this ctxt]
  (let [new-ctxt (do-match* matcher ctxt)]
      (if (= ctxt new-ctxt) nil new-ctxt)))

(defn make-match1-fn [func]
  (fn ([name f] (func name (m1 name f)))
     ([f] (func nil (m1 f)))))

(def m? (make-match1-fn (fn [name f] (Match?. name f))))
(def m* (make-match1-fn (fn [name f] (Match*. name f))))
(def m+ (make-match1-fn (fn [name f] (Match+. name f))))

(defparsertype MatchSeq  [name matchers] [this ctxt]
  (loop [m (first matchers)
         more (rest matchers)
         last-ctxt ctxt]
            (if m (if-let [new-ctxt (match m last-ctxt)] (recur (first more) (rest more) new-ctxt) nil)
                last-ctxt)))

(defparsertype MatchOr  [name matchers] [this ctxt]
  (loop [m (first matchers) more (rest matchers)]
    (when m
      (if-let [new-ctxt (match m ctxt)]
        new-ctxt
        (recur (first more) (rest more))))))

(defn make-match*-fn [func]
  (fn [& forms]
    (let [x (first forms)
          name (when (string? x) x)
          f* (map m1 (if name (rest forms) forms))]
      (func name f*))))

(def mseq (make-match*-fn (fn [name f*] (MatchSeq. name f*))))
(def mor (make-match*-fn (fn [name f*] (MatchOr. name f*))))

(defn do-match-sub-seq [matcher ctxt]
 (let [[coll res] ctxt
      x (first coll)
      more (rest coll)]
   (when (sequential? x)
    (when-let [sub-ctxt (match matcher [x []])]
      (when (done? sub-ctxt) [more (conj res (second sub-ctxt))])))))

(defparsertype MatchSubSeq [name matcher] [this ctxt] (do-match-sub-seq matcher ctxt))

(def msubseq (make-match*-fn (fn [name f*] (MatchSubSeq. name (MatchSeq. name f*)))))

(defparsertype MatchTransform [name transform matcher] [this ctxt]
  (when-let [new-ctxt (match matcher ctxt)]
    (let [orig-res (second ctxt)
          start (count orig-res)
          new-res (second new-ctxt)
          end (count new-res)
          to-transform (log-top " transforming "(subvec new-res start end))
          transformed (log-top " transformed to " (apply transform to-transform))]
      [(first new-ctxt)
       (if transformed
         (if (sequential? transformed) (into orig-res transformed) (conj orig-res transformed))
         orig-res)])))

(defn make-match-transform-fn [func]
  (fn ([name transform m] (func name transform (m1 m)))
     ([transform m] (func nil transform (m1 m)))))

(def mapply (make-match-transform-fn (fn [name transform m] (MatchTransform. name transform m))))
