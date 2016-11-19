(ns speculate.walk
  (:require
   [clojure.spec :as s]
   [speculate.ast :as ast]))

(defn alias [spec]
  (when-let [s (get (s/registry) spec)]
    (when (keyword? s) s)))

(defn push-down-name [name form]
  (cond-> form
    (not (::ast/name form))
    (assoc ::ast/name name)))

(defn node-value
  [{:keys [::ast/name] :as ast}]
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
       (case (::ast/type ast)

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
         (let [{:keys [::ast/name alias leaf form]} ast]
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
