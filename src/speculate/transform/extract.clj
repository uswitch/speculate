(ns speculate.transform.extract
  (:refer-clojure :exclude [alias *])
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [clojure.spec :as s]
   [clojure.spec.override]
   [speculate.ast :as ast]
   [speculate.util :as util]
   [speculate.transform.state :as state]
   [clojure.string :as string]))


(defn alias [spec]
  (when-let [s (get (s/registry) spec)]
    (when (keyword? s) s)))

(defn un-ns [k]
  (keyword (name k)))

(defn assert-conform! [spec orig-value conformed-value]
  (when (= ::s/invalid conformed-value)
    (throw (ex-info "Value doesn't conform-to spec" {:spec spec :value orig-value}))))

(defn leaf-value
  [{:keys [categorize coll-indexes pathset] :as state}
   {:keys [::ast/name] :as ast} node]
  (let [value (s/conform name node)]
    (assert-conform! name node value)
    [[{:label name
       :value (s/unform name value)
       :pathset pathset
       :categorize (cond-> categorize
                     (contains? categorize name)
                     (assoc name #{value}))
       :coll-indexes coll-indexes}] state]))

(defmulti -walk (fn [state ast node] (::ast/type ast)))

(defn walk [state {:keys [leaf] :as ast} node]
  (let [parse-name (::ast/name ast)
        pull-leaf? (and parse-name
                        (not leaf)
                        (contains? (:to-nodeset state) parse-name))]
    (let [inc-alias? (contains? (:include state) parse-name)
          [included] (when inc-alias?
                       (or (leaf-value state ast node)
                           (throw
                            (Exception.
                             (format "Extract keys: Value not present for required key: %s" parse-name)))))
          [pulled s] (when pull-leaf?
                       (-> state
                           (update :pulled (fnil conj #{}) parse-name)
                           (leaf-value ast node)))]
      (if leaf
        (leaf-value state ast node)
        (-> (-walk (if s s state) ast node)
            (state/reset state :categorize)
            (cond->
                included (state/update-value concat included)
                pulled   (state/update-value concat pulled)))))))

(defmethod -walk 'clojure.spec/keys
  [state ast node]
  (let [{:keys [req req-un opt opt-un]} (:form ast)
        f (fn [un? req? s {:keys [leaf] :as branch-ast}]
            (let [label (::ast/name branch-ast)
                  k (cond-> label un? un-ns)
                  s' (-> s
                         (cond-> (not leaf)
                           (update :pathset (fnil conj #{}) label)))]
              (if (contains? node k)
                (let [[result s'' :as value] (walk s' branch-ast (get node k))]
                  (when (and req? (nil? value))
                    (throw
                     (Exception.
                      (format "Value not present for required key: %s" label))))
                  [result (-> s''
                              (assoc :pathset (:pathset s))
                              (cond-> (and value (not leaf))
                                (update :pathset-union (fnil conj #{}) label)))])
                [nil s])))
        [a s] (state/map state (partial f nil :req) req)
        [b t] (state/map state (partial f nil nil) opt)
        [c u] (state/map state (partial f :un :req) req-un)
        [d v] (state/map state (partial f :un nil) opt-un)]
    [(concat a b c d) (util/deep-merge s t u v)]))

(defn -walk-coll [state ast node]
  (let [label (::ast/name ast)
        categorize (:categorize ast)]
    (state/map-indexed state
                       (fn [state i x]
                         (-> (cond-> state
                               (not categorize)
                               (assoc-in [:coll-indexes label] i))
                             (walk (:form ast) x)
                             (state/reset state :coll-indexes)))
                       node)))

(defmethod -walk 'clojure.spec/every
  [state ast node]
  (-walk-coll state ast node))

(defmethod -walk 'clojure.spec/coll-of
  [state ast node]
  (-walk-coll state ast node))

(defn f-or [& fs]
  (comp (partial some identity) (apply juxt fs)))

(defmethod -walk 'clojure.spec/and
  [state ast node]
  (let [specs (filter (f-or (comp s/spec? ::ast/name)
                            (comp util/spec-symbol? ::ast/type))
                      (:form ast))]
    (walk state (first specs) node)))

(defmethod -walk 'clojure.spec/or
  [state ast node]
  (let [label (::ast/name ast)
        conformed (s/conform label node)
        _ (assert-conform! label node conformed)
        [or-key _] conformed
        form (get (:form ast) or-key)]
    (-> state
        (update :categorize assoc or-key #{or-key})
        (update :categories (fnil conj #{}) or-key)
        (update :categorized (partial merge-with set/union) {or-key #{or-key}})
        (walk form node))))

(defn state-fn? [f]
  (-> f meta :name (= 'state-fn)))

(defn key-cat? [[k cat-f]]
  (if (= cat-f keys) :key-cat :set-cat))

(defn include-cat-vals [state set-cats]
  (mapcat (fn [[k vs]]
            (->> vs
                 (mapcat #(when (contains? (s/registry) k)
                            (-> state
                                (leaf-value (ast/parse k) %)
                                (first))))
                 (remove nil?)))
          set-cats))

(defn assert-all-valid! [spec orig-coll conformed-coll]
  (assert
   (not-any? #{::s/invalid} conformed-coll)
   (format "Categorization must be valid %s %s" spec (pr-str orig-coll))))

(defn category-set-map [state set-cat node]
  (->> set-cat
       (map (fn [[k cat-f]]
              (let [cat-set (if (state-fn? cat-f)
                              (cat-f state node)
                              (cat-f node))
                    _ (when (and (not (nil? cat-set)) (empty? cat-set))
                        (throw
                         (IllegalArgumentException.
                          (format "Categorization for %s returned no categories" k))))
                    cat-set' (some->> cat-set
                                      (map #(cond->> %
                                              (or (s/spec? k)
                                                  (contains? (s/registry) k))
                                              (s/conform k)))
                                      (seq)
                                      (set))]
                (assert-all-valid! k cat-set cat-set')
                [k cat-set'])))
       (into {})))

(defn include-not-present [coll-a coll-b]
  (concat coll-a (->> (map :label coll-a)
                      (apply dissoc (group-by :label coll-b))
                      (vals)
                      (apply concat))))

(defn categorize-map [state {:keys [categorize]} form node]
  (let [{:keys [key-cat set-cat]} (group-by key-cat? categorize)
        set-cats (category-set-map state set-cat node)
        state' (-> state
                   (update :categorize merge set-cats)
                   (update :categorized (partial merge-with set/union) set-cats)
                   (update :categories set/union (set (keys categorize))))
        form' (assoc form :categorize categorize)]
    (-> (if key-cat
          (let [[k f] (first key-cat)]
            (state/map state'
                       (fn [s k-cat]
                         (let [k-cat' #{(s/conform k k-cat)}
                               _ (assert-all-valid! k #{k-cat} k-cat')
                               s' (-> s
                                      (update :categorize assoc k k-cat')
                                      (update :categorized update k set/union k-cat'))]
                           (-> s'
                               (walk form' {k-cat (k-cat node)})
                               (state/update-value conj (ffirst (leaf-value s' (ast/parse k) k-cat))))))
                       (f node)))
          (walk state' form' node))
        (state/update-value include-not-present
                            (include-cat-vals state' set-cats)))))

(defn categorize-coll [state ast form nodes]
  (state/map state #(categorize-map %1 ast (:form form) %2) nodes))

(defmethod -walk 'speculate.spec/spec
  [state ast node]
  (let [{:keys [::ast/name alias leaf form select]} ast
        state' (cond-> state alias (update :alias assoc name alias))]
    (-> (cond (:categorize ast)
              (try
                (condp = (::ast/type form)
                  'clojure.spec/keys    (categorize-map state' ast form node)
                  'clojure.spec/coll-of (categorize-coll state' ast form node)
                  'clojure.spec/every   (categorize-coll state' ast form node)
                  (categorize-map state' ast form node))
                (catch IllegalArgumentException _ [[]]))

              select
              (walk state' form node)

              (not leaf)
              (walk state' form node)

              leaf
              (leaf-value state ast node))
        (state/update-state (fn [{:keys [categories categorized pathset-union pulled]}]
                              (assoc state
                                     :pulled pulled
                                     :pathset-union pathset-union
                                     :categories categories
                                     :categorized categorized))))))

(defmethod -walk :default
  [state ast node]
  (leaf-value state ast node))

(defn run-walk [spec node include to-nodeset]
  (walk {:include include :to-nodeset to-nodeset} spec node))

(defn eval-walk [spec node]
  (first (run-walk spec node)))

(defn exec-walk [spec node]
  (second (run-walk spec node)))
