(ns speculate.transform
  (:refer-clojure :exclude [alias *])
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.set :as set]
   [clojure.spec :as s]
   [speculate.ast :as ast]
   [speculate.transform.extract :as tx]
   [speculate.transform.combine :as tc]
   [speculate.util :as util]
   [speculate.walk :as walk]))

(defn state [f]
  (with-meta (fn [state & args] (apply f state args))
    {:name 'state-fn}))

(defn assert-transformable! [from-spec to-spec to-leaves include value-index]
  (let [from-leaf-set (set (map :label value-index))
        from-leaf-set (set (concat (map from-leaf-set include)
                                   from-leaf-set))
        to-leaf-set   (set (map #(or (from-leaf-set (:alias %))
                                     (some-> % :alias-map first key from-leaf-set)
                                     (from-leaf-set (:label %))
                                     (throw
                                      (ex-info (format "From leaf set can't find leaf: %s" (:label %))
                                               {:spec to-spec :leaf %}))) to-leaves))]
    (assert
     (set/subset? to-leaf-set from-leaf-set)
     (format "Cannot transform %s to %s, because the from-spec's
              leaf-set does not contain all the required keys.
              missing: %s"
             from-spec
             to-spec
             (with-out-str
               (pprint (set/difference to-leaf-set from-leaf-set)))))))

(defn transform
  [from-spec to-spec value]
  (assert (s/valid? from-spec value)
          (format "Value does not conform to spec: %s\n%s" from-spec
                  (s/explain from-spec value)))
  (let [to-ast          (ast/parse to-spec)
        to-leaves       (tc/leaves to-ast)
        to-nodeset      (walk/nodeset to-ast)
        include         (->> to-leaves (keep (comp first first :alias-map)) set)
        from-ast        (ast/parse from-spec)
        min-from-ast    (ast/shake to-nodeset from-ast)
        from-nodeset    (walk/nodeset min-from-ast)
        [value-index s] (tx/run-walk min-from-ast value include)
        to-value        (tc/combine value-index
                                    (-> s
                                        (select-keys [:categorized :pathset-union])
                                        (assoc :from-nodeset from-nodeset))
                                    to-ast)]
    (assert (s/valid? to-spec to-value)
            (format "Transformed value does not conform to spec: %s\n%s"
                    to-spec
                   (s/explain to-spec to-value)))
    to-value))
