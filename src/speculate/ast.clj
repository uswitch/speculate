(ns speculate.ast
  (:require
   [clojure.spec :as s]))

(defn named? [x] (instance? clojure.lang.Named x))

(defn ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (if (var? x)
    (let [^clojure.lang.Var v x]
      (symbol (str (.name (.ns v)))
              (str (.sym v))))
    x))

(defn categorize [form]
  (cond (map? form)
        `map?
        (seq? form)
        (first form)
        (s/spec? form)
        `s/spec?
        (var? form)
        `var?
        (set? form)
        `set?
        (named? form)
        `named?))

(defn search
  [pred form]
  (let [rpred #(if (pred %) % (search pred %))]
    (cond
      (list? form) (some rpred form)
      (instance? clojure.lang.IMapEntry form) (rpred (val form))
      (seq? form) (some rpred form)
      (instance? clojure.lang.IRecord form)
      (some rpred form)
      (coll? form) (some rpred form))))

(defmulti parse categorize)

(defmethod parse `s/keys [[t & pairs]]
  (let [form (apply hash-map pairs)]
    {::type t
     :form (-> form
               (update :req    (partial mapv parse))
               (update :req-un (partial mapv parse))
               (update :opt    (partial mapv parse))
               (update :opt-un (partial mapv parse)))}))

(defn kv-form [[t & pairs]]
  {::type t
   :form (->> pairs
              (partition 2)
              (map (juxt first (comp parse second)))
              (into {}))})

(defmethod parse `s/alt     [x] (kv-form x))
(defmethod parse `s/cat     [x] (kv-form x))
(defmethod parse `s/fspec   [x] (kv-form x))
(defmethod parse `s/or      [x] (kv-form x))

(defn pred-forms [[t & preds]]
  {::type t
   :form (map parse preds)})

(defn matches-nilable? [x]
  (when (and (sequential? x)
             (sequential? (second x))
             (sequential? (nth x 2)))
    (let [[_
           [or? knil? snil? pred? _ ]
           [con? second? [fn*? vec? [if? [ssnil?] [kknil?] [ppred?]]]]] x]
      (and (= 'clojure.spec/or or?)
           (= :clojure.spec/nil knil?)
           (= 'clojure.core/nil? snil?)
           (= :clojure.spec/pred pred?)
           (= 'clojure.spec/conformer con?)
           (= 'clojure.core/second second?)
           (= 'fn* fn*?)
           (vector? vec?)
           (= 'if if?)
           (= 'clojure.core/nil? ssnil?)
           (= :clojure.spec/nil kknil?)
           (= :clojure.spec/pred ppred?)))))

(defmethod parse `s/and     [x]
  (if (matches-nilable? x)
    (parse (second x))
    (pred-forms x)))

(defmethod parse `s/tuple   [x] (pred-forms x))

(defn pred-opts-form [[t pred & {:as opts}]]
  (merge opts
         {::type t
          :form (parse pred)}))

(defmethod parse `s/coll-of [x] (pred-opts-form x))
(defmethod parse `s/every   [x] (pred-opts-form x))
(defmethod parse `s/map-of  [x] (pred-opts-form x))

(defmethod parse 'speculate.spec/nillable? [[t form]]
  {::type t :form (parse form)})

(defmethod parse 'speculate.spec/spec [[_ & pairs]]
  (let [{:keys [spec form] :as m} (apply hash-map pairs)]
    (merge {::type 'speculate.spec/spec}
           (when spec {:form (parse spec)})
           (dissoc m :spec :form :alias :categorize :select))))

(defmethod parse 'speculate.spec/strict [[_ merged-keys-form]]
  (parse merged-keys-form))

(defmethod parse 'speculate.spec/map [[t m]]
  {::type t
   :form (->> m
              (map (juxt key (comp parse val)))
              (into {}))})

(defmethod parse `map? [m]
  {::type `map?
   :form (->> m
              (map (juxt key (comp parse val)))
              (into {}))})

(defmethod parse `s/spec? [x]
  (let [tree (-> (s/form x) parse (merge (meta x)))]
    (cond-> tree
      (instance? clojure.lang.IDeref x) (merge (deref x)))))

(defmethod parse `named? [x]
  (if-let [reg (get (s/registry) x)]
    (let [form (parse reg)
          leaf? (not (search ::name form))]
      (cond-> (assoc form ::name x)
        leaf? (assoc :leaf true)))
    (if (symbol? x)
      {::type `symbol? :form x}
      (throw (Exception. (format "Could not find spec in registry: %s" x))))))

(defmethod parse `var? [x]
  (when-let [reg (get (s/registry) (->sym x))]
    (parse reg)))

(defmethod parse `set? [x]
  {::type `set?
   :form x})

(defmethod parse 'clojure.spec/conformer [x])

(defmethod parse :default [x]
  (when x
    (if (and (map? x) (::type x))
     x
     {:form x})))

(defn coll-type? [{:keys [::type]}]
  (contains? #{`s/every `s/coll-of} type))
