(ns clj-parse.core
  (:use [clojure.test]))

(defn- done? [ctxt] (empty? (ctxt 0)))

(defn match1 [test-fn]
  (fn [[coll res]]
    (let [x (first coll)]
      (if (test-fn x)
      [(rest coll) (conj res x)]
      nil))))

(defn matcher? [matcher] (fn [ctxt] (or (matcher ctxt) ctxt)))

(defn match1? [test-fn] (matcher? (match1 test-fn)))

(defn matcher* [matcher]
  (fn [ctxt]
      (loop [last-ctxt ctxt
             cur-ctxt (matcher ctxt)]
        (if cur-ctxt
          (if (done? cur-ctxt) cur-ctxt (recur cur-ctxt (matcher cur-ctxt))) last-ctxt))))

(defn matcher+ [matcher]
  (let [m* (matcher* matcher)]
    (fn [ctxt] (let [new-ctxt (m* ctxt)]
        (if (= ctxt new-ctxt) nil new-ctxt)))))

(defn match* [test-fn] (matcher* (match1 test-fn)))

(defn match+ [test-fn] (matcher+ (match1 test-fn)))

(defn match-seq [& matchers]
  (fn [ctxt]
    (loop [m (first matchers)
           more (rest matchers)
           last-ctxt ctxt]
      (if (and m (not (done? last-ctxt)))
        (if-let [new-ctxt (m last-ctxt)] (recur (first more) (rest more) new-ctxt) nil)
        last-ctxt))))

(defn match-or [& matchers]
  (fn [ctxt]
    (loop [m (first matchers)
           more (rest matchers)]
      (when m
        (if-let [new-ctxt (m ctxt)]
          new-ctxt
          (recur (first more) (rest more)))))))


(defn matches-entirely? [[coll res]]
  (when (empty? coll) res))

(defn match-sub-seq [matcher]
  (fn [ctxt]
    (let [[coll res] ctxt
          x (first coll)
          more (rest coll)]
      (when (seq? x)
        (when-let [[coll2 res2] (matcher [x []])]
          (when (empty? coll2) [more (conj res res2)]))))))

(comment (defn match-transform [test-fn transform]
           (fn [x] (if (test-fn x) (transform x) nil)))) 

(defn match-transform [matcher transform]
  (fn [ctxt]
    (when-let [new-ctxt (matcher ctxt)]
      (let [orig-res (ctxt 1)
            start (count orig-res)
            new-res (new-ctxt 1)
            end (count new-res)]
        (if (= start end)
          new-ctxt
          [(new-ctxt 0) (into orig-res (transform (subvec new-res start end)))])))))

(defn match-ignore [matcher]
  (match-transform matcher (constantly [])))

;; Convenience mini-language definitions

(comment (defmacro || [& body]
           ((match-seq (match1 #(= '| %))
                       (match+ (match-transform seq? #(apply match-seq %)))) [body []])))

(defmacro defmatchseq [& body]
  (let []))


(defn- any [x] true)

(def ?)

(def ||)

(def unary-match-operators #{+ * ?})

(def simple-match-expr (match-transform (match-seq (match1 any) (match1? unary-match-operators))
  (fn [res]
    (let [has-op (= 2 (count res))
          op (when has-op (res 1))
          expr (res 0)]
      [((get {nil match1 ? match1? * match* + match+} op) expr)]))))

(def seq-match-expr (match-transform (matcher+ simple-match-expr)
  (fn [res] [(apply match-seq res)])))

(def or-match-expr (match-transform (match-seq (match-ignore (match1 #(= || %))) (matcher+ (match-sub-seq seq-match-expr)))
  (fn [res] [(apply match-or res)])))

(def match-expr (match-or or-match-expr seq-match-expr))

(defn makematcher [forms]
  (let [matcher (get-in (match-expr [forms []]) [1 0])]
    (fn [coll] (matcher [coll []]))))

(comment (defmacro defmatcher [name body]
           (let [matcher-forms (makematcher body)] `(def ~name ~matcher-forms))))