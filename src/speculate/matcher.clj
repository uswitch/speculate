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
(def A {:a {:a1 1 :a2 2} :b 3})

(s/def ::c (s/keys :req-un [::a]))
(s/def ::B (s/keys :req-un [::c]))
(def B {:c {:a {:a1 1 :a2 2}}})

(s/conform ::A A)
(s/conform ::B B)

(def ast-A (ast/parse ::A))
(def ast-B (ast/parse ::B))

(defmulti children ::ast/type)

(defmethod children `s/keys [ast]
  (reduce concat '() ((juxt :req :req-un :opt :opt-un) (:form ast))))

(defmethod children :default [_] '())

(def decendants
  (fn [ast]
    (let [children (children ast)]
      (apply set/union
             (set (map ::ast/name children))
             (map decendants children)))))

(defmulti pull (fn [ast child] (::ast/type ast)))

(defmethod pull `s/keys [ast child]
  (let [{:keys [req opt]} (:form ast)
        k (::ast/name child)
        namespaced? (contains? (set (map ::ast/name (concat req opt))) k)]
    (if namespaced? k (util/un-ns k))))

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

(def construct nil)
(defmulti construct (fn [_ ast-b] (::ast/type ast-b)))

(defmethod construct `s/keys [ast-a ast-b]
  (if (= ast-a ast-b)
    identity
    (let [children-a (children ast-a)
          bname (::ast/name ast-b)]
      (if-let [child (some (partial = ast-b) children-a)]
        (pull ast-a child)
        (let [dec-a (decendants ast-a)]
          (if (contains? dec-a bname)
            (search-path ast-a bname)
            (let [aname (::ast/name ast-a)
                  dec-b (decendants ast-b)]
              (if (contains? dec-b aname)
                ;; build a b, pass a through to children
                (if (set/intersection dec-a (decendants ast-b))
                  ;; build a b, pass down to children?
                  (let [{:keys [req req-un opt opt-un]} (:form ast-b)]
                    (letfn [(make-kv [i ast] (assoc i (::ast/name ast) (construct ast-a ast)))]
                      (as-> {} %
                        (reduce make-kv % req)
                        (reduce make-kv % req-un)
                        (reduce make-kv % opt)
                        (reduce make-kv % opt-un)))))))))))))

(defmethod construct :default [_ _])

(construct ast-A ast-B)

;; (defn match [ast-a ast-b]
;;   (if (= ast-a ast-b)
;;     identity
;;     (let [cs (children ast-a)]
;;       (if-let [a-eq-b (some (partial = ast-b) cs)]
;;         (match a-eq-b ast-b)
;;         (let [aname (::ast/name ast-a)
;;               bdecn (decendants ast-b)
;;               in-b? (contains? bdecn aname)
;;               ]
;;           (if in-b?
;;             pull
;;             (let [
;;                   c-in-b (filter (fn [x] (contains? bdecn (::ast/name x))) cs)]
;;               (when (seq c-in-b)
;;                 (map #(match % ast-b) c-in-b))))
          
;;           ))))
;;   )

;; (descendants ast-A)
