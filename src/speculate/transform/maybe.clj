(ns speculate.transform.maybe
  (:refer-clojure :exclude [keep seq some some->>]))

(alias 'c 'clojure.core)

(def Nothing (Object.))

(defn seq [x]
  (if (coll? x)
    (if (empty? x) Nothing x)
    x))

(defn nothing? [x]
  (= Nothing x))

(defmacro some->>
  "When expr is not Nothing, threads it into the first form (via ->>),
   and when that result is not Nothing, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nothing? ~g) Nothing (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn keep
  "Like keep, but removes Nothing rather than nil."
  [f coll]
  (lazy-seq
   (when-let [s (c/seq coll)]
     (if (chunked-seq? s)
       (let [c (chunk-first s)
             size (count c)
             b (chunk-buffer size)]
         (dotimes [i size]
           (let [x (f (.nth c i))]
             (when-not (nothing? x)
               (chunk-append b x))))
         (chunk-cons (chunk b) (keep f (chunk-rest s))))
       (let [x (f (first s))]
         (if (nothing? x)
           (keep f (rest s))
           (cons x (keep f (rest s)))))))))

(defn some
  "Like some, but ignores Nothing rather than nil. Returns Nothing if none"
  [pred coll]
  (if (c/seq coll)
    (let [x (pred (first coll))]
      (if (nothing? x)
        (recur pred (next coll))
        x))
    Nothing))
