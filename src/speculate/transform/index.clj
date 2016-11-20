(ns speculate.transform.index
  (:require
   [clojure.set :as set]
   [speculate.util :as util]))

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

(defn val-sets [m]
  (->> m (map (fn [[k v]] [k #{v}])) (into {})))

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

(defn categorize-value-index [categorized categorize? select? value-index]
  (if categorize?
    (cond->> (categorize categorized categorize? value-index)
      select?
      (map #(select categorized select? %)))
    (cond->> value-index
      select?
      (select categorized select?))))
