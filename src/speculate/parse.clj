(ns speculate.parse
  (:require
   [clojure.spec :as s]
   [speculate.parse :as p]))

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

(defmulti ast categorize)

(defmethod ast `s/keys [[t & pairs]]
  (let [form (apply hash-map pairs)]
    {::type t
     :form (-> form
               (update :req    (partial mapv ast))
               (update :req-un (partial mapv ast))
               (update :opt    (partial mapv ast))
               (update :opt-un (partial mapv ast)))}))

(defn kv-form [[t & pairs]]
  {::type t
   :form (->> pairs
              (partition 2)
              (map (juxt first (comp ast second)))
              (into {}))})

(defmethod ast `s/alt     [x] (kv-form x))
(defmethod ast `s/cat     [x] (kv-form x))
(defmethod ast `s/fspec   [x] (kv-form x))
(defmethod ast `s/or      [x] (kv-form x))

(defn pred-forms [[t & preds]]
  {::type t
   :form (map ast preds)})

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

(defmethod ast `s/and     [x]
  (if (matches-nilable? x)
    (ast (second x))
    (pred-forms x)))

(defmethod ast `s/tuple   [x] (pred-forms x))

(defn pred-opts-form [[t pred & {:as opts}]]
  (merge opts
         {::type t
          :form (ast pred)}))

(defmethod ast `s/coll-of [x] (pred-opts-form x))
(defmethod ast `s/every   [x] (pred-opts-form x))
(defmethod ast `s/map-of  [x] (pred-opts-form x))

(defmethod ast 'speculate.spec/nillable? [[t form]]
  {::type t :form (ast form)})

(defmethod ast 'speculate.spec/spec [[_ & pairs]]
  (let [{:keys [spec form] :as m} (apply hash-map pairs)]
    (merge {::type 'speculate.spec/spec}
           (when spec {:form (ast spec)})
           (dissoc m :spec :form :alias :categorize :select))))

(defmethod ast 'speculate.spec/strict [[_ merged-keys-form]]
  (ast merged-keys-form))

(defmethod ast 'speculate.spec/map [[t m]]
  {::type t
   :form (->> m
              (map (juxt key (comp ast val)))
              (into {}))})

(defmethod ast `map? [m]
  {::type `map?
   :form (->> m
              (map (juxt key (comp ast val)))
              (into {}))})

(defmethod ast `s/spec? [x]
  (let [tree (-> (s/form x) ast (merge (meta x)))]
    (cond-> tree
      (instance? clojure.lang.IDeref x) (merge (deref x)))))

(defmethod ast `named? [x]
  (if-let [reg (get (s/registry) x)]
    (let [form (ast reg)
          leaf? (not (search ::name form))]
      (cond-> (assoc form ::name x)
        leaf? (assoc :leaf true)))
    (if (symbol? x)
      {::type `symbol? :form x}
      (throw (Exception. (format "Could not find spec in registry: %s" x))))))

(defmethod ast `var? [x]
  (when-let [reg (get (s/registry) (->sym x))]
    (ast reg)))

(defmethod ast `set? [x]
  {::type `set?
   :form x})

(defmethod ast 'clojure.spec/conformer [x])

(defmethod ast :default [x]
  (when x
    (if (and (map? x) (::type x))
     x
     {:form x})))

(defn coll-type? [{:keys [::type]}]
  (contains? #{`s/every `s/coll-of} type))
