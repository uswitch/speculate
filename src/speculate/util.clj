(ns speculate.util
  (:refer-clojure :exclude [set])
  (:require
   [clojure.string :as string]
   [clojure.walk :as walk]
   [clojure.set :as set]
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]))

(defn alias [spec]
  (when-let [s (get (s/registry) spec)]
    (when (keyword? s) s)))

(defn deep-merge [& xs]
  (cond (every? map? xs)
        (apply merge-with deep-merge xs)
        (every? set? xs)
        (apply set/union xs)
        :else
        (last xs)))

(def spec-symbol?
  '#{clojure.spec/&
     clojure.spec/*
     clojure.spec/+
     clojure.spec/?
     clojure.spec/alt
     clojure.spec/and
     clojure.spec/cat
     clojure.spec/coll-of
     clojure.spec/double-in
     clojure.spec/every
     clojure.spec/every-kv
     clojure.spec/fspec
     clojure.spec/inst-in
     clojure.spec/int-in
     clojure.spec/int-in-range?
     clojure.spec/keys
     clojure.spec/keys*
     clojure.spec/map-of
     clojure.spec/map-spec
     clojure.spec/nilable
     clojure.spec/or
     clojure.spec/regex?
     clojure.spec/spec
     clojure.spec/tuple})

(defn un-ns [k]
  (keyword (name k)))

(defn spec? [x]
  (or (s/spec? x)
      (and (seq? x)
           (spec-symbol? (first x)))))

(defn set [& xs]
  (clojure.core/set xs))

(defn pascal-case [s]
  (->> (string/split s #"-|_|(?=[A-Z])")
       (map string/capitalize)
       (string/join)))

(defn hyphenate [s]
  (some-> s
          (string/split #"_| |(?=[A-Z])")
          (->> (string/join "-"))
          (string/lower-case)))

(defn ->keyword [s]
  (let [[n ns] (reverse (string/split s #"/"))]
    (keyword (when ns (hyphenate ns)) (hyphenate n))))

(defn category [label]
  (cond (sequential? label)
        (keyword (string/join \. (map #(hyphenate (if (keyword? %) (name %) %)) label)))
        (string? label)
        (keyword (hyphenate label))
        :default
        label))

(defn snake-case [s]
 (let [[h & more] (string/split s #"-|_|(?=[A-Z])")]
   (->> (map string/capitalize more)
        (cons (string/lower-case h))
        (string/join))))

(defn snake-case-keys [m]
 (->> m
      (map (fn [[k v]] [(snake-case (cond-> k (keyword? k) name)) v]))
      (into {})))

(defn camel-case [s]
  (let [[h & tail] (string/split s #"-|_|(?=[A-Z])")]
    (string/join (cons (string/lower-case h) (map string/capitalize tail)))))

(defn camel-case-keys [m]
  (->> m
       (map (fn [[k v]] [(camel-case (cond-> k (keyword? k) name)) v]))
       (into {})))

(defn ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (if (var? x)
    (let [^clojure.lang.Var v x]
      (symbol (str (.name (.ns v)))
              (str (.sym v))))
    x))

(defn unfn [expr]
  (if (and (seq? expr)
           (symbol? (first expr))
           (= "fn*" (name (first expr))))
    (let [[[s] & form] (rest expr)]
      (conj (walk/postwalk-replace {s '%} form) '[%] 'fn))
    expr))

(defn res [form]
  (cond
    (keyword? form) form
    (symbol? form) (or (-> form resolve ->sym) form)
    (sequential? form) (walk/postwalk #(if (symbol? %) (res %) %) (unfn form))
    :else form))

(defn named? [x]
  (instance? clojure.lang.Named x))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn kvs-cartesian-product [m]
  (->> m
       (keep (fn [[k vs]] (when (seq vs) (map (fn [v] [k v]) vs))))
       (apply cartesian-product)
       (map (partial into {}))))

(defn rlookup [f]
  (some (fn [[k v]] (and (var? v) (= @v f) k))
        (ns-publics *ns*)))

(defn soft-alias [alias-sym namespace-sym]
  (clojure.core/alias alias-sym
                      (clojure.lang.Namespace/findOrCreate namespace-sym)))

(defmacro ns-alias
  ([alias]
   `(ns-alias ~alias ~alias))
  ([alias namespace-sym]
   `(soft-alias '~alias '~(symbol (format "%s.%s"
                                          (name (.getName *ns*))
                                          (name namespace-sym))))))
