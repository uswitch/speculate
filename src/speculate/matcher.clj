(ns speculate.matcher
  (:refer-clojure :exclude [descendants])
  (:require
   [clojure.spec :as s]
   [speculate.ast :as ast]
   [clojure.set :as set]
   [speculate.util :as util]))

(defmulti children ::ast/type)

(defmethod children `s/keys [ast]
  (let [tag-ns  (fn [x] (assoc x :namespaced? true))
        tag-opt (fn [x] (assoc x :optional? true))
        children-f (juxt (comp (partial map tag-ns) :req)
                         :req-un
                         (comp (partial map (comp tag-opt tag-ns)) :opt)
                         (comp (partial map tag-opt) :opt-un))]
    (reduce concat '() (children-f (:form ast)))))

(defmethod children `s/every [ast] (list (:form ast)))

(defmethod children :default [_] '())

(def decendants
  (fn [ast]
    (let [children (children ast)]
      (apply set/union
             (set (map ::ast/name children))
             (map decendants children)))))

(def extract nil)
(defmulti extract (fn [ast child continue-f] (::ast/type ast)))

(defmethod extract `s/keys [_ child continue-f]
  (let [k (::ast/name child)]
    (comp continue-f (if (:namespaced? child) k (util/un-ns k)))))

(def ^:dynamic *collection-context* false)

(defmethod extract `s/every [_ child continue-f]
  (prn 'extract *collection-context* continue-f)
  (if *collection-context*
    (binding [*collection-context* false] continue-f)
    (fn [coll] (map continue-f coll))))

(defn search-path [ast-a bname continue-f]
  (let [aname (::ast/name ast-a)]
    (if (= aname bname)
      identity
      (let [children-a (children ast-a)
            [child] (filter (comp (partial = bname) ::ast/name) children-a)]
        (if child
          (extract ast-a child continue-f)
          (let [[c] (filter (fn [x]  (contains? (decendants x) bname))
                            children-a)]
            (when c
              (comp continue-f
                    (search-path c bname (extract ast-a c identity))))))))))

(def construct nil)
(defmulti construct (fn [ast-b _] (::ast/type ast-b)))

(defmethod construct :default [_ _] identity)

(defn build-right? [decendants-b name-a children-a children-b]
  (or (contains? decendants-b name-a)
      (seq (set/intersection (set (map ::ast/name children-a))
                             (set (map ::ast/name children-b))))))

(declare match)

(defn match-decendants [ast-left children-left]
  (fn [child-right]
    (let [[child-a :as matched]
          (filter (fn [child-left]
                    (set/intersection (decendants child-left)
                                      (decendants child-right)))
                  children-left)]
      (if (= (count matched) 1)
        [(extract ast-left child-a (match child-a child-right))
         (::ast/name child-right)
         child-right]
        [(match ast-left child-right)
         (::ast/name child-right)
         child-right]))))

(defmethod construct `s/keys [ast-b fs]
  (fn [value]
    (reduce (fn [i [f k ast]]
              (let [k (if (:namespaced? ast) k (util/un-ns k))]
                (assoc i k (f value)))) {} fs)))

(defmethod construct `s/every [ast-b [[f _ ast]]]
  (binding [*collection-context* true]
    (fn [value]
      (prn 'construct 's/every value)
      (map f value))))

(defmacro debug [label expr]
  `(do
     (prn '~label)
     ~expr))

(defn match [ast-a ast-b]
  (if (= ast-a ast-b)
    (debug match1 identity)
    (let [children-a (children ast-a)
          bname (::ast/name ast-b)]
      (if-let [child (first (filter (partial = ast-b) children-a))]
        (debug match2 (extract ast-a child identity))
        (let [dec-a (decendants ast-a)]
          (if (contains? dec-a bname)
            (debug match3 (search-path ast-a bname identity))
            (let [aname (::ast/name ast-a)
                  dec-b (decendants ast-b)
                  children-b (children ast-b)]
              (if (build-right? dec-b aname children-a children-b)
                (debug
                 match4
                 (->> children-b
                      (map (juxt (partial match ast-a) ::ast/name identity))
                      (construct ast-b)))
                (when (set/intersection dec-a dec-b)
                  (debug
                   match5
                   (->> children-b
                        (map (match-decendants ast-a children-a))
                        (construct ast-b))))))))))))

