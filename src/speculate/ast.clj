(ns speculate.ast
  (:refer-clojure :exclude [alias])
  (:require
   [clojure.spec :as s]
   [speculate.util :as util]))

(defn alias [spec]
  (when-let [s (get (s/registry) spec)]
    (when (keyword? s) s)))

(defn node-value
  [{:keys [::name] :as ast}]
  (if name
    [{:label name
      :alias (alias name)
      :alias-map (:alias ast)}]
    []))

(defrecord Walked [value])

(defn walked [value]
  (->Walked value))

(defn walk [f ast]
  (let [value (f ast)]
    (if (instance? Walked value)
      (:value value)
      (concat
       value
       (case (::type ast)

         clojure.spec/keys
         (let [{:keys [req req-un opt opt-un]} (:form ast)]
           (mapcat (partial walk f) (concat req req-un opt opt-un)))

         clojure.spec/every
         (walk f (:form ast))

         clojure.spec/coll-of
         (walk f (:form ast))

         clojure.spec/or
         (mapcat (comp (partial walk f) val) (:form ast))

         clojure.spec/and
         (mapcat (partial walk f) (:form ast))

         speculate.spec/spec
         (let [{:keys [::name alias leaf form]} ast]
           (cond alias
                 (f ast)
                 (not leaf)
                 (walk f form)
                 leaf
                 (f ast)))

         [])))))

(defn leaves [ast]
  (distinct (walk (fn [{:keys [leaf] :as ast}]
                    (if leaf (node-value ast) []))
                  ast)))

(defn leafset [ast]
  (->> ast
       (leaves)
       (mapcat (juxt (comp ffirst :alias-map) :alias :label))
       (remove nil?)
       (set)))

(defn nodes [ast]
  (walk node-value ast))

(defn nodeset [ast]
  (->> ast
       (nodes)
       (mapcat (juxt (comp ffirst :alias-map) :alias :label))
       (remove nil?)
       (set)))

(defn shake [keepset {:keys [::name] :as ast}]
  (if (and name
           (or (contains? keepset name)
               (contains? keepset (ffirst (::alias ast)))
               (contains? keepset (alias name))))
    ast
    (case (::type ast)
      clojure.spec/keys
      (let [{:keys [req req-un opt opt-un]} (:form ast)
            req    (seq (keep (partial shake keepset) req))
            req-un (seq (keep (partial shake keepset) req-un))
            opt    (seq (keep (partial shake keepset) opt))
            opt-un (seq (keep (partial shake keepset) opt-un))
            form   (cond-> nil
                     req    (assoc :req req)
                     req-un (assoc :req-un req-un)
                     opt    (assoc :opt opt)
                     opt-un (assoc :opt-un opt-un))]
        (when form
          (assoc ast :form form)))
      clojure.spec/or
      (some->> (:form ast)
               (keep (juxt first (comp (partial shake keepset) second)))
               (seq)
               (into {})
               (assoc ast :form))
      clojure.spec/and
      (let [spec? #(or (util/spec-symbol? (::type %))
                       (s/spec? (::name %)))]
        (some->> (:form ast)
                 (filter spec?)
                 (keep (partial shake keepset))
                 (seq)
                 (assoc ast :form)))
      speculate.spec/spec
      (let [{:keys [leaf]} ast]
        (some->> ast :form (shake keepset) (assoc ast :form)))
      (if (:leaf ast)
        (let [name (::name ast)]
          (when (or (contains? keepset name)
                    (contains? keepset (alias name)))
            ast))
        (some->> ast :form (shake keepset) (assoc ast :form))))))

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
        (util/named? form)
        `util/named?))

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
(defmethod parse `s/or      [x] (assoc (kv-form x) :spec x))

(defn pred-forms [[t & preds]]
  {::type t
   :form (map parse preds)})

(s/def ::nilable-if
  (s/and seq?
         (s/cat :if #{'if}
                :nil? (s/and seq?
                             (s/cat :nil? #{`nil?}
                                    :gensym symbol?))
                :nil #{[::s/nil nil]}
                :pred (s/tuple #{::s/pred} any?))))

(s/def ::nilable-conformer
  (s/and seq?
         (s/cat :conformer #{`s/conformer}
                :second #{`second}
                :fn* (s/and seq?
                            (s/cat :fn* #{'fn*}
                                   :vector (s/coll-of symbol?
                                                      :max-count 1
                                                      :min-count 1
                                                      :kind vector?)
                                   :if ::nilable-if)))))

(s/def ::nilable-form
  (s/cat :and #{`s/and}
         :or (s/and seq?
                    (s/cat :or #{`s/or}
                           :nil #{::s/nil}
                           :nil? #{`nil?}
                           :pred #{::s/pred}
                           :sym any?))
         :conformer ::nilable-conformer))

(defn matches-nilable? [x]
  (s/valid? ::nilable-form x))

(defn nilable-pred [x]
  (-> x second (nth 4)))

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

(defmethod parse 'speculate.spec/spec [[_ & pairs]]
  (let [{:keys [spec form] :as m} (apply hash-map pairs)]
    (merge {::type 'speculate.spec/spec}
           (when spec {:form (parse spec)})
           (dissoc m :spec :form :alias :categorize :select))))

(defmethod parse 'speculate.spec/strict [[_ merged-keys-form]]
  (parse merged-keys-form))

(defmethod parse `map? [m]
  {::type `map?
   :form (->> m
              (map (juxt key (comp parse val)))
              (into {}))})

(defn strip-reader-meta [form]
  (dissoc form :line :column))

(defmethod parse `s/spec? [x]
  (let [tree (-> (s/form x)
                 (parse)
                 (merge (strip-reader-meta (meta x))))]
    (cond-> tree
      (instance? clojure.lang.IDeref x) (merge (deref x)))))

(def complex-type?
  (comp '#{clojure.spec/coll-of
           clojure.spec/every
           clojure.spec/keys
           clojure.spec/tuple
           clojure.spec/map-of} ::type))

(defn leaf? [ast]
  (not (search (fn [x]
                 (and (complex-type? x)
                      (or (::name x)
                          (search ::name x))))
               ast)))

(defmethod parse `util/named? [x]
  (if-let [reg (get (s/registry) x)]
    (let [form (parse reg)]
      (cond-> (assoc form ::name x)
        (leaf? form) (assoc :leaf true)))
    (if (symbol? x)
      {::type `symbol? :form x}
      (throw (Exception. (format "Could not find spec in registry: %s" x))))))

(defmethod parse `var? [x]
  (when-let [reg (get (s/registry) (util/->sym x))]
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
