(ns speculate.transform.combine
  (:refer-clojure :exclude [* alias])
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [clojure.spec :as s]
   [speculate.ast :as ast]
   [speculate.transform.maybe :as maybe]
   [speculate.util :as util]))

(defn alias [spec]
  (when-let [s (get (s/registry) spec)]
    (when (keyword? s) s)))

(defn push-down-name [name form]
  (cond-> form
    (not (::ast/name form))
    (assoc ::ast/name name)))

(defn leaf-value
  [{:keys [::ast/name] :as ast}]
  (assert name (format "Cannot be a leaf without a name: %s" (pr-str ast)))
  [{:label name
    :alias (alias name)
    :alias-map (:alias ast)}])

(defmulti -leaves #(if (:leaf %) :default (::ast/type %)))

(defmethod -leaves 'clojure.spec/keys
  [ast]
  (let [{:keys [req req-un opt opt-un]} (:form ast)]
    (mapcat -leaves (concat req req-un opt opt-un))))

(defmethod -leaves 'clojure.spec/every
  [ast]
  (-leaves (:form ast)))

(defmethod -leaves 'clojure.spec/coll-of
  [ast]
  (-leaves (:form ast)))

(defmethod -leaves 'clojure.spec/or
  [ast]
  (mapcat (comp -leaves val) (:form ast)))

(defmethod -leaves 'speculate.spec/spec
  [{:keys [::ast/name alias leaf form] :as ast}]
  (let [form' (push-down-name name form)]
    (cond alias
          (leaf-value ast)
          (not leaf)
          (-leaves form')
          leaf
          (leaf-value ast))))

(defmethod -leaves :default
  [ast]
  (leaf-value ast))

(defn leaves [ast] (distinct (-leaves ast)))

(defn assert-conform! [spec value]
  (when-not (s/valid? spec value)
    (throw
     (ex-info "Value doesn't conform-to spec" {:spec spec :value value})))
  value)

(defn or-coerce [spec k]
  (let [k' (s/conform spec k)]
    (when-not (= ::s/invalid k') k')))

(defn f-or [& fs]
  (comp (partial some identity) (apply juxt fs)))

(defn unwrap [spec value]
  (cond (nil? value)
        maybe/Nothing
        (map? value)
        (:value value)
        (sequential? value)
        (let [[v :as vs] (map :value value)]
          (cond (s/valid? spec vs)
                vs
                (s/valid? spec v)
                v
                :else
                (throw (ex-info "Leaf value did not match spec"
                                {:label spec :value value}))))))

(defn get-by [value-index & ks]
  (some (fn [{:keys [label] :as value}]
          (when (contains? (set ks) label) value))
        value-index))

(defn combine-leaf-value
  [value-index {:keys [::ast/name] :as ast}]
  (if-let [[[k txfn]] (seq (:alias ast))]
    (if-let [v (get-by value-index k)]
      (txfn (unwrap k v))
      maybe/Nothing)
    (if-let [v (get-by value-index name (alias name))]
      (unwrap name v)
      maybe/Nothing)))

(def -combine nil)
(defmulti -combine (fn [value-index _ ast] (::ast/type ast)))

(defn combine [value-index index-meta ast]
  (if (:leaf ast)
    (combine-leaf-value value-index ast)
    (-combine value-index index-meta ast)))

(defn filter-when [cond pred coll]
  (cond->> coll cond (filter pred)))

(defn get-category-value [value-index name]
  (let [maybe-val (some->> value-index
                           (some (comp name :categorize))
                           first)]
    (when-not (= name maybe-val) maybe-val)))

(defn restrict-to-path-category
  [{:keys [pathset-union from-nodeset]} name value-index]
  (cond (and (contains? pathset-union name)
             (contains? from-nodeset name))
        (->> value-index
             (filter (fn [{:keys [pathset]}]
                       (contains? pathset name)))
             (seq))
        ;; (contains? from-nodeset name)
        ;; nil
        :else
        value-index))

(defn build-kv-fn
  [value-index {:keys [categorized] :as index-meta}]
  (fn [construct req? empty {:keys [::ast/name leaf] :as form}]
    (if-let [category-value (get-category-value value-index name)]
      (construct name category-value)
      (let [value-index' (if leaf
                           value-index
                           (restrict-to-path-category index-meta name value-index))
            value (combine value-index' index-meta form)
            value (cond (maybe/nothing? value)
                        maybe/Nothing
                        (and (= :opt req?)
                             ('#{clojure.spec/coll-of
                                 clojure.spec/every} (::parse/type form))
                             (empty? value))
                        maybe/Nothing
                        :else value)]
        (if (maybe/nothing? value)
          (empty name)
          (->> value
               (assert-conform! name)
               (construct name)))))))

(defmethod -combine 'clojure.spec/keys
  [value-index index-meta {:keys [keys?] :as ast}]
  (let [{:keys [req req-un opt opt-un]} (:form ast)
        keys?-pred (some-> value-index first :categorize (get keys?))
        pred       (comp keys?-pred ::ast/name)
        un-pred    (comp keys?-pred
                         (f-or (partial or-coerce keys?) util/un-ns)
                         ::ast/name)
        construct  vector
        unstruct   (fn [name value] [(util/un-ns name) value])
        opt-empty  (constantly maybe/Nothing)
        req-empty  (constantly maybe/Nothing)
        build-kv   (build-kv-fn value-index index-meta)]
    (maybe/some->>
     (concat (->> req
                  (filter-when keys?-pred pred)
                  (maybe/keep (partial build-kv construct :req req-empty)))
             (->> opt
                  (filter-when keys?-pred pred)
                  (maybe/keep (partial build-kv construct :opt opt-empty)))
             (->> req-un
                  (filter-when keys?-pred un-pred)
                  (maybe/keep (partial build-kv unstruct :req req-empty)))
             (->> opt-un
                  (filter-when keys?-pred un-pred)
                  (maybe/keep (partial build-kv unstruct :opt opt-empty))))
     (maybe/seq)
     (into {}))))

(defn val-sets [m]
  (->> m (map (fn [[k v]] [k #{v}])) (into {})))

(def cartesian-product (memoize util/kvs-cartesian-product))

(defn expand-value-index [categorized]
  (mapcat (fn [{:keys [categorize] :as v}]
            (->> categorize
                 (merge categorized)
                 (cartesian-product)
                 (map #(assoc v :categorize %))))))

(defn restrict-keys
  "Like select-keys where all keys are a map, ignores keys not present"
  [m keyseq]
  (let [pred (fn [map-key]
               (some (fn [seq-key]
                       (= map-key (select-keys seq-key (keys map-key))))
                     keyseq))]
    (->> m (filter (comp pred key)) (into {}))))

(defn merge-into-set [a b]
  (reduce (fn [init [k v]]
            (update init k (fnil conj #{}) (k b)))
          a b))

(defn compress [value-index]
  (->> value-index
       (group-by #(dissoc % :categorize :pathset))
       (map (fn [[k vs]]
              (assoc k
                     :categorize (->> vs
                                      (map :categorize)
                                      (reduce merge-into-set {}))
                     :pathset (->> vs (map :pathset) (reduce set/union)))))))

(defn categorize [categorized categorize value-index]
  (let [keys-to-expand       (keys categorize)
        minimal-categorized  (select-keys categorized keys-to-expand)
        expanded-categorized (cartesian-product categorized)
        expand-nil-to-all    (partial merge-with #(or %2 %1) minimal-categorized)
        expand-xform         (comp (map #(update % :categorize expand-nil-to-all))
                                   (expand-value-index categorized))
        group                (memoize #(select-keys % keys-to-expand))]
    (->> value-index
         (sequence expand-xform)
         (group-by (comp group :categorize))
         (#(restrict-keys % expanded-categorized))
         (vals)
         (map (comp #(with-meta % {:categorized (:categorize (first %))})
                    compress)))))

(def leaf?
  (memoize
   (fn [ast]
     (->> (leaves ast)
          (mapcat (juxt :label :alias (comp ffirst :alias-map)))
          (remove nil?)
          (set)))))

(defn only-leaves [ast value-index]
  (filter (comp (leaf? ast) :label) value-index))

(defn coll-combine [value-index index-meta ast]
  (let [spec (::ast/name ast)
        form (:form ast)
        coll-index-f #(get-in % [:coll-indexes spec])
        apply-to-all (remove coll-index-f value-index)
        indexed (->> value-index
                     (filter coll-index-f)
                     (group-by coll-index-f)
                     (sort-by key)
                     (vals))
        coll-into (condp = (::s/kind-form ast)
                    'clojure.core/set? #{}
                    'clojure.core/list? (list)
                    [])]
    (if (seq indexed)
      (maybe/some->> indexed
                     (map (partial concat apply-to-all))
                     (maybe/keep #(combine % index-meta form))
                     (maybe/seq)
                     (into coll-into))
      (if-let [cat? (:categorize form)]
        (maybe/some->> value-index
                       (categorize (:categorized index-meta) cat?)
                       (maybe/keep #(combine % index-meta (:form ast)))
                       (maybe/seq)
                       (into coll-into))
        (maybe/some->> value-index
                       (only-leaves ast)
                       (group-by :coll-indexes)
                       (vals)
                       (maybe/keep #(combine % index-meta (:form ast)))
                       (into coll-into))))))

(defmethod -combine 'clojure.spec/every
  [value-index index-meta ast]
  (coll-combine value-index index-meta ast))

(defmethod -combine 'clojure.spec/coll-of
  [value-index index-meta ast]
  (coll-combine value-index index-meta ast))

(defmethod -combine 'clojure.spec/and
  [value-index index-meta ast]
  (->> (:form ast)
       (filter (f-or (comp s/spec? ::ast/name)
                     (comp util/spec-symbol? ::ast/type)))
       (maybe/some (fn [v]
                     (let [v' (combine value-index index-meta v)]
                       (if (s/valid? (::ast/name ast) v')
                         v'
                         maybe/Nothing))))))

(defmethod -combine 'clojure.spec/or
  [value-index index-meta {:keys [form] :as ast}]
  (let [ks (set (keys form))]
    (maybe/some (fn [[k v]]
                  (let [value-index' (remove (fn [{:keys [categorize] :as value}]
                                               (seq (select-keys categorize
                                                                 (disj ks k))))
                                             value-index)
                        v' (combine value-index' index-meta v)]
                    (if (s/valid? (::ast/name ast) v')
                      v'
                      maybe/Nothing)))
                form)))

(defn select [{:keys [categorized]} select value-index]
  (let [categorized (or (-> value-index meta :categorized) categorized)
        f (->> select
               (map (fn [[k f]]
                      (cond (fn? f)  f
                            (set? f) (comp f k :categorize))))
               (apply juxt)
               (comp (partial every? some?)))
        xform (comp (expand-value-index categorized)
                    (filter f))
        filtered (sequence xform value-index)]
    (with-meta (compress filtered)
      {:categorized (merge categorized
                           (-> (first filtered)
                               (:categorize)
                               (select-keys (keys select))
                               (val-sets)))})))

(defn key-cat? [[k cat-f]]
  (if (= cat-f keys) :key-cat :set-cat))

(defn valmerge [a b]
  (condp (fn [f [a b]] (and (f a) (f b))) [a b]
    map? (merge-with valmerge a b)
    seq? (distinct (concat a b))
    set? (set/union a b)
    b))

(defn categorize-value-index [categorized categorize? select? value-index]
  (if categorize?
    (cond->> (categorize categorized categorize? value-index)
      select?
      (map #(select categorized select? %)))
    (cond->> value-index
      select?
      (select categorized select?))))

(defmethod -combine 'speculate.spec/spec
  [value-index {:keys [categorized] :as index-meta} ast]
  (let [{:keys [alias form leaf]} ast
        form' (push-down-name (::ast/name ast) form)
        categorize? (:categorize ast)
        {[key-cat] :key-cat} (group-by key-cat? categorize?)
        form-type (condp = (::ast/type form)
                    'clojure.spec/keys    :map
                    'clojure.spec/coll-of :coll
                    'clojure.spec/every   :coll
                    :other)
        value-index' (->> value-index
                          (categorize-value-index categorized
                                                  categorize?
                                                  (:select ast)))
        cat-fn (fn [x] (assoc index-meta
                              :categorized (or (-> x meta :categorized)
                                               categorized)))]
    (cond (empty? value-index')
          maybe/Nothing
          (or alias leaf)
          (combine-leaf-value value-index' ast)
          :default
          (if categorize?
            (if (= form-type :map)
              (if key-cat
                (let [[k v] key-cat]
                  (maybe/some->> value-index'
                                 (maybe/keep #(combine % (cat-fn %) (assoc form' :keys? k)))
                                 (maybe/seq)
                                 (reduce merge {})))
                (maybe/some->> value-index'
                               (maybe/keep #(combine % (cat-fn %) form'))
                               (maybe/seq)
                               (reduce valmerge)))
              (maybe/some->> value-index'
                             (maybe/keep #(combine % (cat-fn %) form'))
                             (maybe/seq)))
            (combine value-index' (cat-fn value-index') form')))))

(defmethod -combine :default
  [value-index _ ast]
  (combine-leaf-value value-index ast))
