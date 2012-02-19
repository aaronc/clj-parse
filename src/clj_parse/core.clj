;; ## Overview
;;
;; A simple framework for creating parsers that parse
;; (and possibly transform) sequences of clojure symbols
;; into sequences of clojure symbols.  This could be useful
;; for generating macros, functions with complex syntax, or
;; even parsing text if the text has been tokenized into clojure
;; symbols.
;;
;; ## Usage
;;
;; Parsers are created using 8 core functions called matchers
;; prefixed with m: m1, m?, m*, m+, mseq, mor, mapply, and msubseq.
;; The matcher names should be descriptive enough to give some idea.
;;
;; Matchers are either composed of other matchers or test functions
;; that take a single token and return a boolean result.  If an
;; array is passed to a match function #(apply mseq %) is called on
;; that array.  All match functions taken an optional name argument
;; as their first argument to be used in debugging.
;;
;; m1, m?, m*, m+ take one matcher argument.  mseq, mor, and msubseq take a
;; variable number of matcher arguments.
;;
;; mapply takes one matcher argument and a transform function which
;; can transform the the output of its child matcher if that matcher
;; succeeds.
(ns clj-parse.core)


;; ## Definitions for debug logging

(def ^{:dynamic true :doc "Bound to the function used for debug logging or nil to disable logging"} **debug**  nil)

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

(defmacro dbg-parser
  "Convenience macro for debugging parsers.  Wrap the expressions
  you want to debug in the macro.  Ex: `(dbg-parser (my-parser tokens))`"
  [& body]
  `(binding [**debug** println log-stack (atom (list))] ~@body))

(defmacro dbg-parser* 
  "Same as `dbg-parser` excepts requires that you specify a `debug-redirect` function
  to redirect debugging output to as the first parameter.  `dbg-parser`
  defaults to using `println`."
  [debug-redirect & body]
  `(binding [**debug** ~debug-redirect log-stack (atom (list))] ~@body))

;; Definitions of the core parser functions and data structures

(defprotocol IMatcher
  "The core interface of the parsing framework.  All functions which do
  parsing implement IMatcher.  Any function implementing IMatcher
  can be used as an expression in a parser and any parser generated
  by the matcher functions implements IMatcher."
  
  (match [this ctxt]
    "The function called to perform matches. The ctxt parameter is a
  vector of two sequences [tokens output] - the sequence of tokens to
  be parsed and the resulting sequence of output tokens.  Any implementation
  of the match function should return either a ctxt vector if the match was
  successful or nil if the match failed.  Tokens that have been parsed
  should be removed from the token list and any result tokens should be
  added to the output list (these may be the same tokens that were
  parsed or transformations of them).  The same ctxt vector can even
  be returned to indicate that a match was successful but had no side
  effects."))

(defn parse "Given an IMatcher matcher and a sequence of tokens,
  parse the tokens and return the result sequence or nil if the
  matcher did not match all of the tokens."
  [matcher tokens] (let [[coll res] (match matcher [tokens []])]
                             (when (empty? coll) res)))

(defmacro defparsertype
  "Used for defining IMatcher instances, ensuring that their match
  function is wrapped within logging code for debugging purposes and
  exception handlers.  Also adds an implementation of clojure.lang.IFn
  to IMatcher's that calls the `parse` function (so that `(parse myParser tokens)`
  and `(myParser tokens) are equivalent."
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

;; Calls test-fn on the first token in the token sequence.
;; If test-fn is not nil or false, takes the token from
;; the token sequence and places it in the output sequence.
(defparsertype Match1 
  [name test-fn] [this [coll res]] (when-let [x (first coll)]
          (if (test-fn x)
            [(rest coll) (conj res x)]
            nil)))

(declare mseq)

(defn- assoc-name [name m] (if name (assoc m :name name) m))

(defn m1
  "Matches the given matcher once and fails otherwise.
  matcher can be an IMatcher instance, a test function,
  or a sequence of arguments for mseq as described above."
  ([name matcher]
     (if (satisfies? IMatcher matcher)
       (assoc-name name matcher)
       (if (sequential? matcher)
         (assoc-name name (apply mseq matcher))
         (Match1. name matcher))))
  ([matcher] (m1 nil matcher)))

(defparsertype Match? [name matcher] [this ctxt] (or (match matcher ctxt) ctxt))

(defn- do-match* [matcher ctxt]
    (loop [last-ctxt ctxt]
    (if (done? last-ctxt)
      last-ctxt
      (if-let [cur-ctxt (match matcher last-ctxt)]
                            (if (= cur-ctxt last-ctxt) last-ctxt (recur cur-ctxt))
                            last-ctxt))))

(defparsertype Match* [name matcher] [this ctxt] (do-match* matcher ctxt))

(defparsertype Match+ [name matcher] [this ctxt]
  (let [new-ctxt (do-match* matcher ctxt)]
      (if (= ctxt new-ctxt) nil new-ctxt)))

(defn make-match1-fn [func]
  (fn ([name f] (func name (m1 name f)))
     ([f] (func nil (m1 f)))))

(def match1-arglists '([name matcher] [matcher]))

(def ^{:doc "Matches the given matcher once or succeeds with no side effects." :arglists '([name matcher] [matcher])}
  m? (make-match1-fn (fn [name f] (Match?. name f))))
(def ^{:doc "Matches the given matcher any number of times or succeeds with no side effects." :arglists '([name matcher] [matcher])}
  m* (make-match1-fn (fn [name f] (Match*. name f))))
(def ^{:doc "Matches the given matcher one or more times or fails." :arglists '([name matcher] [matcher])}
  m+ (make-match1-fn (fn [name f] (Match+. name f))))

(defparsertype MatchSeq [name matchers] [this ctxt]
  (loop [m (first matchers)
         more (rest matchers)
         last-ctxt ctxt]
            (if m (if-let [new-ctxt (match m last-ctxt)] (recur (first more) (rest more) new-ctxt) nil)
                last-ctxt)))

(defparsertype MatchOr [name matchers] [this ctxt]
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

(def ^{:doc "Creates a matcher which matchs the given matchers in order." :arglists '([name & matchers] [& matchers])}
  mseq (make-match*-fn (fn [name f*] (MatchSeq. name f*))))
(def ^{:doc "Creates a matcher which tries to match any one of the given matchers, returning the transformation of the
first successful matcher." :arglists '([name & matchers] [& matchers])}
  mor (make-match*-fn (fn [name f*] (MatchOr. name f*))))

(defn do-match-sub-seq [matcher ctxt]
 (let [[coll res] ctxt
      x (first coll)
      more (rest coll)]
   (when (sequential? x)
    (when-let [sub-ctxt (match matcher [x []])]
      (when (done? sub-ctxt) [more (conj res (second sub-ctxt))])))))

(defparsertype MatchSubSeq [name matcher] [this ctxt] (do-match-sub-seq matcher ctxt))

(def ^{:doc "Creates a matcher which if it encounters a sequence as the first token,
  will try to match the sequence of matchers against the tokens in the sub-sequence, failing
  if the matchers do not parse the subsequence tokens completely, and
  appending the array of results to the result sequence as an array." :arglists '([name & matchers] [& matchers])}
  msubseq (make-match*-fn (fn [name f*] (MatchSubSeq. name (MatchSeq. name f*)))))

(defparsertype MatchTransform [name transform matcher] [this ctxt]
  (when-let [new-ctxt (match matcher ctxt)]
    (let [orig-res (second ctxt)
          start (count orig-res)
          new-res (second new-ctxt)
          end (count new-res)
          to-transform (log-top " transforming "(subvec new-res start end))
          transformed (log-top " transformed to " (apply transform to-transform))]
      [(first new-ctxt)
       (if (not (nil? transformed))
         (if (sequential? transformed) (into orig-res transformed) (conj orig-res transformed))
         orig-res)])))

(defn make-match-transform-fn [func]
  (fn ([name transform m] (func name transform (m1 m)))
     ([transform m] (func nil transform (m1 m)))))

(def
  ^{:doc "Calls #(apply transform %) on the sequence of results appended to the result array by matcher
  if matcher succeeds and fails otherwise.

  The transform function should take the number of arguments
  it expects to receive from the matcher (or a variable number if it doesn't know.  If the transform
  function returns nil or an empty sequence, the match still succeeds, but nothing is appended to the
  result array (i.e. the result is ignored).  If the transform function returns a sequence,
  the items from the sequence are appended into the result array.  If the transform function returns
  any other value, that value is appended to the result array.  (To append nil to the result array,
  transform should return [nil]."}
  mapply (make-match-transform-fn (fn [name transform m] (MatchTransform. name transform m))))
