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
;; that array.  If a literal is passed to a match function, that
;; literal is matched exactly.  Keywords and symbols are treated as
;; literals.  All match functions taken an optional name argument as
;; their first argument to be used in debugging.
;;
;; m1, m?, m*, m+ take one matcher argument.  mseq, mor, and msubseq take a
;; variable number of matcher arguments.
;;
;; mapply takes one matcher argument and a transform function which
;; can transform the the output of its child matcher if that matcher
;; succeeds.
(ns clj-parse.core)

;; ## Definitions for debug logging and error handling

(def ^{:dynamic true :doc "Bound to the function used for debug logging or nil to disable logging"} **debug**  nil)

(def ^:private ^:dynamic parser-stack nil)

(def ^:private ^:dynamic cur-tokens nil)

(def ^:private ^:dynamic error-info nil)

(def ^:private ^:dynamic error-index nil)

(def ^:private ^:dynamic nested-exception nil)

(defn- log-top [action ctxt]
  (do (when **debug**
        (let [top (peek @parser-stack)
              depth (count @parser-stack)
              pre (apply str (repeat (dec depth) "-"))
              ctxt (if (nil? ctxt) "nil" ctxt)
              name (when top (str "<" (.Name (class top)) " "
                                (or (:name top) "Unknown") ">"))]
          (when (> depth 0)
            (**debug** (str pre name action ctxt)))))
      ctxt))
        
(defn- push-log [this ctxt]
  (do (log-top " passed " ctxt)
      (swap! parser-stack conj this)))

(defn- pop-log [ret] (do (log-top " returned " ret) (swap! parser-stack pop)))

(defmacro dbg-parser
  "Convenience macro for debugging parsers.  Wrap the expressions
  you want to debug in the macro.  Ex: `(dbg-parser (my-parser tokens))`"
  [& body]
  `(binding [**debug** println parser-stack (atom (list))] ~@body))

(defmacro dbg-parser* 
  "Same as `dbg-parser` excepts requires that you specify a `debug-redirect` function
  to redirect debugging output to as the first parameter.  `dbg-parser`
  defaults to using `println`."
  [debug-redirect & body]
  `(binding [**debug** ~debug-redirect log-stack (atom (list))] ~@body))

;; ## Definitions of the core parser functions and data structures

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

;; These functions are used to interact with the parsing context
(defn- ctxtcreate [tokens] [tokens []])

(defn- cpeek [ctxt] (ffirst ctxt))

(defn- cpop [[tokens res]] [(rest tokens) res])

(defn- cpop-conj [[tokens res] x] [(rest tokens) (conj res x)])

(defn- csetresult [[tokens res1] res2] [tokens res2])

(defn- ctokens [ctxt] (first ctxt))

(defn- cresult [ctxt] (second ctxt))

(defn- cdone? [ctxt] (empty? (first ctxt)))

(defn- track-match-fail! [ctxt]
  (when error-index
    (let [last-idx @error-index
          cur-idx (count (first ctxt))]
       (cond (< cur-idx last-idx)
                (do (swap! error-info (constantly [[ctxt @parser-stack]]))
                    (swap! error-index (constantly cur-idx))
                    (swap! nested-exception (constantly nil)))
             (= cur-idx last-idx)
                (swap! error-info conj [ctxt @parser-stack]))
                nil)))

(defmacro wrap-track-fail [ctxt & body]
  `(let [res# (do ~@body)] (if res# res# (track-match-fail! ~ctxt))))

(defmacro protect-shared-globals [tokens & body]
  `(if (and cur-tokens (not (identical? cur-tokens ~tokens)))
     (binding [cur-tokens ~tokens error-info nil error-index nil]
       (do ~@body))
     (do ~@body)))

(defn parse "Given an IMatcher matcher and a sequence of tokens,
  parse the tokens and return the result sequence or nil if the
  matcher did not match all of the tokens."
  [matcher tokens] (protect-shared-globals tokens
                     (let [ctxt (match matcher (ctxtcreate tokens))] 
                       (when (cdone? ctxt) (cresult ctxt)))))

(defn- infer-token-name [pstack] (let [top (peek pstack)] (or (:name top) (pr-str top))))

(defn parse-detailed "Run parse as above but when the parse fails
  instead of returning nil, return a map with information detailing
  the error cause."
  [matcher tokens]
  (binding
      [cur-tokens tokens
       parser-stack (or parser-stack (atom (list)))
       error-info (atom [])
       error-index (atom (count tokens))
       nested-exception (atom nil)]
    (let [res-ctxt (match matcher (ctxtcreate tokens))]
      (cond
       (nil? res-ctxt) ; Parsing failed completely
          (let [ntokens (count tokens)
                idx (- ntokens @error-index)
                error-details @error-info
                eof (= idx ntokens)
                token (when (not eof) (nth tokens idx))]
          {:error-details error-details
           :index idx
           :eof eof
           :actual-token token
           :expected-tokens (map #(infer-token-name (second %)) error-details)
           :nested-exception @nested-exception})
       (not (cdone? res-ctxt)) ; Parsing was successful, but not all of
          {:unexpected-tokens (ctokens res-ctxt)} ; the tokens were parsed
       :success
          (cresult res-ctxt)))))

(defn create-parser-error-msg [err-map]
  (if-let [unexpected (:unexpected-tokens err-map)]
    (str "Unexpected tokens: " (into [] unexpected))
    (let [expected (:expected-tokens err-map)]
      (str
       (if (:eof err-map) "Unexpected end of input. "
           (str "Got " (:actual-token err-map) " at index "
                (:index err-map) ". "))
       "Expected" (when (> 1 (count expected) " one of"))
       ": " (apply str (interpose ", " expected))
       (when-let [ex (:nested-exception err-map)]
         (str "\nPossibly caused by caught exception: " ex))))))

(defn parse-ex
  "Same as parse function above, but throws an exception
  when the parser fails."
  [matcher tokens]
  (try (let [res (parse-detailed matcher tokens)]
         (if (map? res)
           (throw (ex-info (str (or (:name matcher) "Parser") " error. "
                                (create-parser-error-msg res))
                           {:type ::parse-exception :data res}
                           (:nested-exception res)))
           res))
       (catch Object ex
         (let [ex-map (ex-data ex)]
           (if (= (:type (ex-data ex)) ::parse-exception)
             (throw ex)
             (throw (ex-info (str "Caught exception in "
                                  (or (:name matcher) "parser") ": " (str ex))
                             {:type ::parse-exception :data {:nested-exception ex}}
                             ex)))))))

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
           (try (when clj-parse.core/parser-stack (clj-parse.core/push-log ~this ~ctxt))
                      (let [ret# (do ~@body)]
                        (when clj-parse.core/parser-stack (clj-parse.core/pop-log ret#))
                        ret#)
                      (catch Exception e#
                        (when clj-parse.core/parser-stack
                          (clj-parse.core/log-top " received exception:" "")
                          (when clj-parse.core/**debug** (clj-parse.core/**debug** e#))
                          (clj-parse.core/track-match-fail! ~ctxt))
                        (when clj-parse.core/nested-exception
                          (swap! clj-parse.core/nested-exception (constantly e#)))
                        nil)))
           clojure.lang.IFn
           (clojure.lang.IFn/invoke [this# forms#] (clj-parse.core/parse-ex this# forms#))))


;; Calls test-fn on the first token in the token sequence.
;; If test-fn is not nil or false, takes the token from
;; the token sequence and places it in the output sequence.
(defparsertype Match1 [name test-fn] [this ctxt]
  (wrap-track-fail ctxt (when (not (cdone? ctxt))
          (let [x (cpeek ctxt)]
            (when (test-fn x) (cpop-conj ctxt x))))))

(declare mseq)

(defn- assoc-name [name m] (if name (assoc m :name name) m))

(declare m1)

(defn mlit "Creates a matcher which makes the specified literal
  exactly once.  If name is nil, the matcher name is the literal
  itself - this is useful for debugging and errors messages."
  ([name literal] (m1 (if name name (str literal)) #(= literal %)))
  ([literal] (mlit nil literal)))

(defn m1
  "Matches the given match-expr once and fails otherwise.
  match-expr can be an IMatcher instance, a test function
  taking one parameter and returning a true value if the match
  is successful, a sequence of arguments for mseq as described above,
  or a literal value. Keywords and symbols even though they
  implement IFn are treated as literals."
  ([name match-expr]
     (if (satisfies? IMatcher match-expr)
       (assoc-name name match-expr)
       (if (sequential? match-expr)
         (assoc-name name (apply mseq match-expr))
         (if (and (ifn? match-expr)
                  (not (keyword? match-expr))
                  (not (symbol? match-expr)))
           (Match1. name match-expr)
           (mlit name match-expr)))))
  ([match-expr] (m1 nil match-expr)))

(defparsertype MatchQ [name matcher] [this ctxt] (or (match matcher ctxt) ctxt))

(defn- do-match* [matcher ctxt]
    (loop [last-ctxt ctxt]
    (if (cdone? last-ctxt)
      last-ctxt
      (if-let [cur-ctxt (match matcher last-ctxt)]
                            (if (= cur-ctxt last-ctxt) last-ctxt (recur cur-ctxt))
                            last-ctxt))))

(defparsertype MatchStar [name matcher] [this ctxt] (do-match* matcher ctxt))

(defparsertype MatchPlus [name matcher] [this ctxt]
  (let [new-ctxt (do-match* matcher ctxt)]
      (if (= ctxt new-ctxt) nil new-ctxt)))

(defn make-match1-fn [func]
  (fn ([name f] (func name (m1 name f)))
     ([f] (func nil (m1 f)))))

(def ^{:doc "Matches the given matcher once or succeeds with no side effects." :arglists '([name matcher] [matcher])}
  m? (make-match1-fn (fn [name f] (MatchQ. name f))))
(def ^{:doc "Matches the given matcher any number of times or succeeds with no side effects." :arglists '([name matcher] [matcher])}
  m* (make-match1-fn (fn [name f] (MatchStar. name f))))
(def ^{:doc "Matches the given matcher one or more times or fails." :arglists '([name matcher] [matcher])}
  m+ (make-match1-fn (fn [name f] (MatchPlus. name f))))

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

(defparsertype MatchTransform [name transform matcher] [this ctxt]
  (when-let [new-ctxt (match matcher ctxt)]
    (let [orig-res (cresult ctxt)
          start (count orig-res)
          new-res (cresult new-ctxt)
          end (count new-res)
          to-transform (log-top " transforming "(subvec new-res start end))
          transformed (log-top " transformed to " (apply transform to-transform))]
          (csetresult new-ctxt
                      (if (nil? transformed)
                        orig-res
                        (if (sequential? transformed)
                          (into orig-res transformed) (conj orig-res transformed)))))))

(defn- cput [[tokens res] x] [tokens
 (if (sequential? x) (into res x) (conj res x))])

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
  any other value, that value is conj'ed to the result array.  (To append nil to the result array,
  transform should return [nil]."}
  mapply (make-match-transform-fn (fn [name transform m] (MatchTransform. name transform m))))

(defparsertype MatchSub [name sub-parser] [this ctxt]
  (wrap-track-fail ctxt (when (not (cdone? ctxt))
                         (let [res (sub-parser (cpeek ctxt))]
                           (log-top " sub parser returned " res)
                           (when (not (nil? res)) (cpop-conj ctxt res))))))

(defn msub "Allows a sub-parser to parse a single expression and
  substitute it into the result stream.  sub-parser should be a function
  which takes one argument.  If it returns nil, the match fails.
  Otherwise, its result is conj'ed onto the result sequence."
  ([name sub-parser] (MatchSub. name sub-parser))
  ([sub-parser] (msub nil sub-parser)))

(defparsertype Match1Extended [name match-fn] [this ctxt]
  (wrap-track-fail ctxt (when (not (cdone? ctxt))
          (let [x (cpeek ctxt)]
            (when (match-fn x ctxt) (cpop-conj ctxt x))))))

(defn m1extended
  "Same as m1, but here match-fn takes two parameters:
the token to be tested and the current parse context."
  ([name match-fn] (Match1Extended. name match-fn))
  ([match-fn] (m1extended nil match-fn)))
