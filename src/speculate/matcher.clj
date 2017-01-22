(ns speculate.matcher
  (:refer-clojure :exclude [descendants])
  (:require
   [clojure.spec :as s]
   [speculate.ast :as ast]
   [clojure.set :as set]
   [speculate.util :as util]))

(s/def ::a1 #{1})
(s/def ::a2 #{2})
(s/def ::b  #{3})
(s/def ::a (s/keys :req-un [::a1 ::a2]))
(s/def ::A (s/keys :req-un [::a ::b]))
(s/def ::A2 (s/keys :req-un [::A]))
(def A {:a {:a1 1 :a2 2} :b 3})
(def A2 {:A A})

(s/def ::c (s/keys :req-un [::a]))
(s/def ::B (s/keys :req-un [::c]))
(s/def ::B2 (s/keys :req-un [::B]))
(def B {:c {:a {:a1 1 :a2 2}}})
(def B2 {:B B})

(s/conform ::A A)
(s/conform ::B B)

(def ast-A (ast/parse ::A))
(def ast-B (ast/parse ::B))

(defmulti children ::ast/type)

(defmethod children `s/keys [ast]
  (let [tag-ns  (fn [x] (assoc x :namespaced? true))
        tag-opt (fn [x] (assoc x :optional? true))
        children-f (juxt (comp (partial map tag-ns) :req)
                         :req-un
                         (comp (partial map (comp tag-opt tag-ns)) :opt)
                         (comp (partial map tag-opt) :opt-un))]
    (reduce concat '() (children-f (:form ast)))))

(defmethod children :default [_] '())

(def decendants
  (fn [ast]
    (let [children (children ast)]
      (apply set/union
             (set (map ::ast/name children))
             (map decendants children)))))

(defmulti pull (fn [ast child] (::ast/type ast)))

(defmethod pull `s/keys [_ child]
  (let [k (::ast/name child)]
    (if (:namespaced? child) k (util/un-ns k))))

(defmulti search-path (fn [ast-a bname] (::ast/type ast-a)))

(defmethod search-path `s/keys [ast-a bname]
  (let [aname (::ast/name ast-a)]
    (if (= aname bname)
      identity
      (let [children-a (children ast-a)
            [child] (filter (comp (partial = bname) ::ast/name) children-a)]
        (if child
          (pull ast-a child)
          (let [[c] (filter (fn [x]  (contains? (decendants x) bname))
                            children-a)]
            (when c
              (comp (search-path c bname) (pull ast-a c)))))))))

(comment

  (= ((search-path (ast/parse ::B) ::a) B) {:a1 1, :a2 2})

  )

(defn build-right? [decendants-b name-a children-a children-b]
  (or (contains? decendants-b name-a)
      (seq (set/intersection (set (map ::ast/name children-a))
                             (set (map ::ast/name children-b))))))

(defn match-decendants [ast-left children-left]
  (fn [child-right]
    (let [[child-a :as matched]
          (filter (fn [child-left]
                    (set/intersection (decendants child-left)
                                      (decendants child-right)))
                  children-left)]
      (if (= (count matched) 1)
        [(comp (construct child-a child-right) (pull ast-left child-a))
         (::ast/name child-right)
         child-right]
        [(construct ast-left child-right)
         (::ast/name child-right)
         child-right]))))

(def construct nil)
(defmulti construct (fn [ast-b _] (::ast/type ast-b)))

(defmethod construct `s/keys [ast-b fs]
  (fn [value]
    (reduce (fn [i [f k ast]]
              (let [k (if (:namespaced? ast) k (util/un-ns k))]
                (assoc i k (f value)))) {} fs)))

(defn match [ast-a ast-b]
  (if (= ast-a ast-b)
    identity
    (let [children-a (children ast-a)
          bname (::ast/name ast-b)]
      (if-let [child (first (filter (partial = ast-b) children-a))]
        (pull ast-a child)
        (let [dec-a (decendants ast-a)]
          (if (contains? dec-a bname)
            (search-path ast-a bname)
            (let [aname (::ast/name ast-a)
                  dec-b (decendants ast-b)
                  children-b (children ast-b)]
              (if (build-right? dec-b aname children-a children-b)
                (->> children-b
                     (map (juxt (partial construct ast-a) ::ast/name identity))
                     (construct ast-b))
                (when (set/intersection dec-a dec-b)
                  (->> children-b
                       (map (match-decendants ast-a children-a))
                       (construct ast-b)))))))))))

(defmethod construct :default [_ _] identity)

(comment

  (require '[clojure.test :refer [deftest is]])

  (deftest matcher-test
    (is (= identity (match ast-A ast-A)))

    (is (= :a (match (ast/parse ::c) (ast/parse ::a))))

    (let [f (match ast-B (ast/parse ::a))]
      (is (= (f B) {:a1 1 :a2 2})))

    (is (= ((match (ast/parse ::a) (ast/parse ::c)) {:a1 1 :a2 2})
           {:a {:a1 1, :a2 2}})) 

    (is (= ((match (ast/parse ::A) (ast/parse ::c)) A) {:a {:a1 1, :a2 2}})) 

    (is (= B ((match ast-A ast-B) A)))

    (is (= ((match (ast/parse ::A2) (ast/parse ::B2)) A2) B2)))

  )
(matcher-test)



