(ns clojure.spec.override
  (:require
   [clojure.spec :refer :all]))

(in-ns 'clojure.spec)

(alias 'c 'clojure.core)

(defn ^:skip-wiki every-impl
  "Do not call this directly, use 'every', 'every-kv', 'coll-of' or 'map-of'"
  ([form pred opts] (every-impl form pred opts nil))
  ([form pred {gen-into :into
               :keys [kind ::kind-form count max-count min-count distinct gen-max ::kfn
                      conform-keys ::conform-all]
               :or {gen-max 20}
               :as opts}
    gfn]
     (let [conform-into gen-into
           check? #(valid? pred %)
           kfn (c/or kfn (fn [i v] i))
           addcv (fn [ret i v cv] (conj ret cv))
           cfns (fn [x]
                  ;;returns a tuple of [init add complete] fns
                  (cond
                   (c/and (vector? x) (c/or (not conform-into) (vector? conform-into)))
                   [identity
                    (fn [ret i v cv]
                      (if (identical? v cv)
                        ret
                        (assoc ret i cv)))
                    identity]

                   (c/and (map? x) (c/or (c/and kind (not conform-into)) (map? conform-into)))
                   [(if conform-keys empty identity)
                    (fn [ret i v cv]
                      (if (c/and (identical? v cv) (not conform-keys))
                        ret
                        (assoc ret (nth (if conform-keys cv v) 0) (nth cv 1))))
                    identity]
                  
                   (c/or (list? conform-into) (c/and (not conform-into) (list? x)))
                   [(constantly ()) addcv reverse]

                   :else [#(empty (c/or conform-into %)) addcv identity]))]
       (reify
        Spec
        (conform* [_ x]
                  (cond
                   (coll-prob x kind kind-form distinct count min-count max-count
                              nil nil nil)
                   ::invalid

                   conform-all
                   (let [[init add complete] (cfns x)]
                     (loop [ret (init x), i 0, [v & vs :as vseq] (seq x)]
                       (if vseq
                         (let [cv (dt pred v nil)]
                           (if (= ::invalid cv)
                             ::invalid
                             (recur (add ret i v cv) (inc i) vs)))
                         (complete ret))))
                   
                   
                   :else
                   (if (indexed? x)
                     (let [step (max 1 (long (/ (c/count x) *coll-check-limit*)))]
                       (loop [i 0]
                         (if (>= i (c/count x))
                           x
                           (if (check? (nth x i))
                             (recur (c/+ i step))
                             ::invalid))))
                     (c/or (c/and (every? check? (take *coll-check-limit* x)) x)
                           ::invalid))))
         (unform* [_ x]
           ;; Presume this is a collection for ease
           (letfn [(maybe-unform [x]
                     (cond (and (not (fn? pred))
                                (contains? (registry) pred))
                           (unform pred x)
                           (satisfies? clojure.spec/Spec pred)
                           (unform pred x)
                           :else x))]
             (cond (vector? x)
                   (mapv maybe-unform x)
                   (seq? x)
                   (map maybe-unform x)
                   (set? x)
                   (set (map maybe-unform x))
                   :default x)))
        (explain* [_ path via in x]
                  (c/or (coll-prob x kind kind-form distinct count min-count max-count
                                   path via in)
                        (apply concat
                               ((if conform-all identity (partial take *coll-error-limit*))
                                (keep identity
                                      (map (fn [i v]
                                             (let [k (kfn i v)]
                                               (when-not (check? v)
                                                 (let [prob (explain-1 form pred path via (conj in k) v)]
                                                   prob))))
                                           (range) x))))))
        (gen* [_ overrides path rmap]
              (if gfn
                (gfn)
                (let [pgen (gensub pred overrides path rmap form)]
                  (gen/bind
                   (cond
                    gen-into (gen/return (empty gen-into))
                    kind (gen/fmap #(if (empty? %) % (empty %))
                                   (gensub kind overrides path rmap form))
                    :else (gen/return []))
                   (fn [init]
                     (gen/fmap
                      #(if (vector? init) % (into init %))
                      (cond
                       distinct
                       (if count
                         (gen/vector-distinct pgen {:num-elements count :max-tries 100})
                         (gen/vector-distinct pgen {:min-elements (c/or min-count 0)
                                                    :max-elements (c/or max-count (max gen-max (c/* 2 (c/or min-count 0))))
                                                    :max-tries 100}))

                       count
                       (gen/vector pgen count)

                       (c/or min-count max-count)
                       (gen/vector pgen (c/or min-count 0) (c/or max-count (max gen-max (c/* 2 (c/or min-count 0)))))

                       :else
                       (gen/vector pgen 0 gen-max))))))))
        
        (with-gen* [_ gfn] (every-impl form pred opts gfn))
        (describe* [_] `(every ~form ~@(mapcat identity opts)))))))
