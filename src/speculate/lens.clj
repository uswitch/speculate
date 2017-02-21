(ns speculate.lens
  (:require
   [bifocal.combinator :as com]
   [bifocal.functor :refer [ffilter fmap]]
   [bifocal.lens :as lens]
   [clojure.spec :as s]
   [speculate.ast :as ast]
   [speculate.spec :as u]
   [speculate.util :as util]
   [clojure.set :as set]
   [speculate.spec :as spec]
   [speculate.transform.state :as state]))

(defmulti -mk ::ast/type)

(defn mk [{:keys [leaf ::ast/name] :as ast}]
  (-mk ast)
  #_(if (:leaf ast)
    (lens/meta (fn [s] (merge (meta s) {:name name})))
    (-mk ast)))

(defmethod -mk `s/keys [ast]
  (let [descend (fn [lens-f]
                  (fn [ast]
                    (comp (lens-f ast) (mk ast))))
        {:keys [req req-un opt opt-un]} (:form ast)]
    (comp (apply lens/+>>
                 (concat
                  (map (descend (comp lens/key ::ast/name)) req)
                  (map (descend (comp lens/key util/un-ns ::ast/name)) req-un)
                  (map (descend (comp lens/key ::ast/name)) opt)
                  (map (descend (comp lens/key util/un-ns ::ast/name)) opt-un)))
          lens/flatten)))

(defmethod -mk `s/or [ast]
  (apply lens/cond
         (map (fn [[_ {:keys [::s/name] :as ast}]]
                [(partial s/valid? name) (mk ast)])
              (:form ast))))

(defmethod -mk `s/and [ast]
  (->> (:form ast)
       (filter (fn [{:keys [::s/name ::ast/type]}]
                 (or (s/spec? name) (util/spec-symbol? type))))
       (first)
       (mk)))

(defmethod -mk `s/every [ast]
  (comp lens/map (-> ast :form mk)))

(defmethod -mk `s/nilable [ast]
  (lens/maybe (mk (:form ast))))

(defmethod -mk `s/tuple [ast]
  (apply lens/tuple lens/+>> (map mk (:form ast))))

(defmethod -mk `u/categorize [ast]
  (comp (lens/meta (fn [s]
                     (->> (:categorize ast)
                          (map (fn [[k f]] [k (f s)]))
                          (into {}))))
        (mk (:form ast))))

(defmethod -mk `u/select [ast]
  (let [next (mk (:form ast))
        pred (fn [s]
               (every? (fn [[k f]] (f (k (meta s))))
                       (:select ast)))]
    (fn [f]
      (fn
        ([s]
         (f (ffilter pred (lens/flat-map #(lens/view next %) s))))
        ([s g] )))))

(defmethod -mk :default [_] lens/id)

(def A {:a "a"
        :b "b"
        :c {:e 7
            :f [{:g #{:a} :h 9 :i {:j "j" :k '[a b c]}}
                {:g #{:b :c} :h 10 :i {:j "j" :k '[a b c]}}
                {:g #{:d} :h 11 :i {:j "j" :k '[a b c]}}]}
        :a2 {:a3 "a3"}})

(s/def ::A (s/keys :req-un [::a ::b ::c ::a2]))
(s/def ::a3 #{"a3"})
(s/def ::a2 (s/keys :req-un [::a3]))
(s/def ::a #{"a"})
(s/def ::b #{"b"})
(s/def ::c (s/keys :req-un [::e ::f]))
(s/def ::e #{7})
(s/def ::f (s/coll-of ::gn))
(s/def ::gn (s/keys :req-un [::g ::h ::i]))
(s/def ::g (s/coll-of #{:a :b :c :d} :kind set?))
(s/def ::h #{9 10 11})
(s/def ::i (s/keys :req-un [::j ::k]))
(s/def ::j #{"j"})
(s/def ::k #{'[a b c]})

(s/def ::B (s/coll-of ::an))
(s/def ::an (s/keys :req-un [::a ::b ::e ::g ::h :i2/i]))
(s/def :i2/i (s/keys :req-un [::j]))

;; (defn common-branches [a b]
;;   (let [c-a (set (children a))
;;         c-b (set (children b))]
;;     (set/intersection c-a c-b)))

;; (defn uncommon-branches [a b]
;;   (let [c-a (set (children a))
;;         c-b (set (children b))]
;;     (set/difference c-a c-b)))

(defn key-for-ast [{:keys [::ast/name key-type]}]
  (case key-type
    :req name
    :req-un (util/un-ns name)
    :opt name
    :opt-un (util/un-ns name)))

(def equal?
  "True if two specs are the same value"
  =)

(defn equivalent?
  "An equivalence between data structures defined by two specs exists
   if the specs can be proven to describe the same structure"
  [a b]
  false)

(defn isomorphic?
  "An isomorphism between data structures defined by two specs exists
   if all leaves of one spec can be represented in another, with
   regard to categorizations"
  [a b]
  (= (set (ast/categorized-leaves a)) ; not complete, need to remove selects
     (set (ast/categorized-leaves b))))

(defn type-equiv? [{:keys [::ast/type]} b]
  (when (= type (::ast/type b)) type))

(defn child-intersection? [a b]
  (set/intersection (set (ast/children a)) (set (ast/children b))))

(defn descendent-intersection? [a b]
  (set/intersection (ast/descendants a) (ast/descendants b)))


;; Are these useful?
;; (defn child-type-equiv? [a b])
;; (defn descendent-type-equiv? [a b])

;; Branch Conditions
;; 1.  = Equivalence
;; 2.  ≅ Isomorphism - test this how?
;; 3.    Type similarity, E.G., 2x{} 2x[]
;; 4.    Intersection of children
;; 5.    Intersection of descendants
;; 6.    Type similarity of children
;; 7.    Type similarity of descendants
;; 8.    Destination leads hierarchy


;; (let [a (ast/parse (s/keys :req-un [::a ::b]))
;;       b (ast/parse (s/keys :req-un [::a]))
;;       db (descendants b)
;;       drop (->> (children a)
;;                 (remove #(seq (set/intersection db (descendants %))))
;;                 (map key-for-ast))]
;;  (lens/view (apply com/dissoc drop) {:a 1 :b 2})
;;   ;; drop
;;   )

;; (defn which [child-a ast-b]
;;   (let [children-a (->> (children child-a)
;;                         (map (juxt ::ast/name identity))
;;                         (into {}))
;;         children-a-set (set (map ::ast/name (vals children-a)))
;;         children-b (children ast-b)
;;         children-b-set (set (map ::ast/name children-b))
;;         name-a (::ast/name child-a)
;;         key-a (key-for-ast child-a)
;;         promote (set/intersection children-a-set children-b-set)]
;;     (cond (contains? children-b-set name-a)
;;           lens/id
;;           (contains? (descendants ast-b) name-a)
;;           lens/id
;;           (seq promote)
;;           (->> promote
;;                (map (comp key-for-ast children-a))
;;                (apply com/promote-only key-a))
;;           ;; (set/intersection (descendants child-a) (descendants ast-b))
;;           :else
;;           lens/id)))

(defn derive-isomorphism [a b])

(defn iso-or-eq? [a b]
  (or (equal? a b)
      (equivalent? a b)
      (isomorphic? a b)))

(defn hoist [ast])

(defn hoist-path [ast path])

(defn plunge [ast])

(defn plunge-path [ast path])

;; * categorize implies a collection where order is not important, type is
;;   although, a key-categorized map must count as a collection here

;; * groupings on keys must be specified, path to leaf is assumed irrelevant

;; * lack of categorize implies a collection where index is important,
;;   or at least groups the leaves below

(defn keep-children [ast matching-children])

(declare build)

(defn match-descendants [a b]
  (let [children-a    (ast/children a)
        children-b    (ast/children b)
        child-match   (set/intersection children-a children-b)
        match-names   (set (concat (keep ::ast/name child-match)
                                   (mapcat ast/descendants child-match)))
        descendants-a (ast/descendants a)
        descendants-b (ast/descendants b)
        descend-match (set/intersection descendants-a descendants-b)]
    (if (seq child-match)
      (let [keep-lens (keep-children a child-match)]
        (if (= match-names descend-match)
          keep-lens
          (apply comp keep-lens (map #(build % b) children-a))
          ;; ^^ don't think this is correct
          ))
      (when (seq descend-match)
        (apply comp (keep #(build % b) children-a))))))

(defn coll-spec? [{:keys [::ast/type]}]
  (case type
    clojure.spec/every true
    clojure.spec/coll-of true
    nil))

(defn categorized-coll? [ast]
  (when (coll-spec? ast)
    (->> ast ast/children first ::ast/type (= `u/categorize))))

(defn path-elem [{:keys [::ast/type ::ast/name] :as ast}]
  (case type
    clojure.spec/keys name
    nil))

(defn find-matching-collection [a b]
  (let [descendants-b (ast/descendants b)]
    (find-path (fn [x]
                 (and (coll-spec? x)
                      (set/intersection (ast/descendants x) descendants-b)))
               a)))

;; (find-matching-collection (ast/parse ::A) (ast/parse ::B))

;; we're missing a bit of logic to hoist collections, or at least to
;; be able to recognize when a collection should be hoisted
(defn build [a b]
  (cond
    (equal? a b)
    lens/id
    (equivalent? a b)
    lens/id
    (and (coll-spec? a) (coll-spec? b))
    lens/id
    (categorized-coll? b)
    (find-matching-category a b)
    (isomorphic? a b)
    (derive-isomorphism a b)
    :else
    (let [children-a (set (ast/children a))]
      (if-let [match (get children-a b)]
        (hoist match)
        (let [children-b (set (ast/children b))]
          (if-let [match (get children-b a)]
            (plunge match)
            (let [descendants-a (ast/descendants a)]
              (if-let [match (get descendants-a (::ast/name b))]
                (hoist-path a (find-path a match))
                (let [descendants-b (ast/descendants b)]
                  (if-let [match (get descendants-b (::ast/name a))]
                    (plunge-path b (find-path b match))
                    (match-descendants a b)))))))))))

(defn walk-state* [state f [node & nodes]]
  (let [[value state' :as return] (f state node)]
    (if (ast/walked? value)
      [(:value value) state']
      (let [children (ast/children node)]
        (recur state' f (concat children nodes))))))

(defn walk-states [state f [node & nodes]]
  (let [[value state' :as return] (f state node)]
    (if (ast/walked? value)
      [(:value value) state']
      (let [children (ast/children node)]
        (if (seq children)
          (concat (walk-states state' f children)
                  (walk-states state f nodes))
          (when (seq nodes)
            (walk-states state f nodes)))))))

(defn walk-state [state f ast]
  (walk-state* state f [ast]))

(defn find-path [pred ast]
  (let [[value found?]
        (walk-states []
                     (fn [path {aname ::ast/name :as ast}]
                       (if (pred ast)
                         [(ast/walked (conj path aname)) ::found-path]
                         (if-let [path-elem (path-elem ast)]
                           [ast (conj path path-elem)]
                           [ast path])))
                     (ast/children ast))]
    (when (= ::found-path found?) value)))

(defn hoist-target [pred ast]
  (let [[value found?]
        (walk-states lens/id
                     (fn [lens ast]
                       (if (pred ast)
                         [(ast/walked ) ::target-found]
                         )
                       )
                     (ast/children ast))]
    (when (= ::target-found found?) value)))

(defn postwalk [f {atype ::ast/type :as ast}]
  (case atype
    clojure.spec/keys
    (f (-> ast
           (update-in [:form :req]    (partial keep (partial postwalk f)))
           (update-in [:form :req-un] (partial keep (partial postwalk f)))
           (update-in [:form :opt]    (partial keep (partial postwalk f)))
           (update-in [:form :opt-un] (partial keep (partial postwalk f)))))
    clojure.spec/or
    (f (update ast :form (partial fmap (partial postwalk f))))
    clojure.spec/and
    (f (update-in ast [:form 0] (partial postwalk f)))
    clojure.spec/every
    (f (update ast :form (partial postwalk f)))
    clojure.spec/coll-of
    (f (update ast :form (partial postwalk f)))
    clojure.spec/nilable
    (f (update ast :form (partial postwalk f)))
    clojure.spec/tuple
    (f (update ast :form (partial keep (partial postwalk f))))
    (f ast)))

(defn shake [keep ast]
 (postwalk (fn [x]
             (if (contains? keep (::ast/name x))
               x
               (when (seq (remove nil? (ast/children x))) x)))
           ast))

(defn extract [input-ast return-ast]
  {:pre [(= `s/tuple (::ast/type return-ast))]}
  (let [return (ast/children return-ast)
        leaves (map ::ast/name return)
        inputs (map #(shake #{%} input-ast) leaves)]
    (apply lens/+> (map mk inputs))
    ;; (last inputs)
    ))

;; (extract (ast/parse ::A) (ast/parse (s/tuple ::a3 ::a ::b ::i)))

(lens/view (extract (ast/parse ::A)
                    (ast/parse (s/tuple ;::a3 ::a ::b
                                ::i
                                )))
           A)

;; Give up on this, query lenses are the only hope.
;; What is a query anyway?
;; getting a tuple from a ds? Getting a 
;; A query is to pull values into a tuple to x-apply - no inner transformation
;; So, the lens/insert into tuple should work.
;; there's a special case here, we can cheat. Iterate over the tuple,
;; find-paths, hoist
;; (s/tuple ::a (u/select ::b :x #{"this"}))
;; what if ::a returns a collection?
;; i.e., parallel combinators /should/ concat, but traversals should
;; return collections?

;; so the tactic will be:
;; parse the spec-A, and the tuple-spec-B
;; shake A so that only the leaves in B remain
;; if we assume that we can only hoist, hoist-path, list_map it should work?

;; if find-path encounters a collection, then hoist-path cannot work
;; we've missed a bit around predictively finding collections ahead of
;; us, and hoisting them first

;; (let [a (ast/parse (s/keys :req-un [::a ::c ::b]))
;;       b (ast/parse (s/keys :req-un [::a ::e ::f]))
;;       cb (set (map ::ast/name (children b)))
;;       lenses (->> (children a) (map #(which % b)))]
;;   (lens/view (apply comp lenses)
;;              {:a "a" :b "b" :c {:e 7 :f :ff}})
;;   )

;; (descendants (ast/parse (s/keys :req-un [::a ::b])))

;; (= (lens/view (comp (com/promote-key :c)
;;                     (com/promote-sequence :f)
;;                     (com/split :g #(assoc % :g %2))
;;                     lens/map
;;                     (com/dissoc-in [:i] :k))
;;               A)
;;    '({:g :a :h 9  :i {:j "j"} :a "a" :b "b" :e 7}
;;      {:g :c :h 10 :i {:j "j"} :a "a" :b "b" :e 7}
;;      {:g :b :h 10 :i {:j "j"} :a "a" :b "b" :e 7}
;;      {:g :d :h 11 :i {:j "j"} :a "a" :b "b" :e 7}))
