(ns speculate.matcher-test
  (:require [speculate.matcher :as matcher]
            [clojure.test :refer [deftest is]]
            [speculate.ast :as ast]
            [clojure.spec :as s]))

(s/def ::a1 #{1})
(s/def ::a2 #{2})
(s/def ::b  #{3})
(s/def ::a (s/keys :req-un [::a1 ::a2]))
(s/def ::A (s/keys :req-un [::a ::b]))
(s/def ::A2 (s/keys :req-un [::A]))
(def A {:a {:a1 1 :a2 2} :b 3})
(def A2 {:A A})
(s/def ::a3 (s/coll-of ::A2))

(s/def ::c (s/keys :req-un [::a]))
(s/def ::B (s/keys :req-un [::c]))
(s/def ::B2 (s/keys :req-un [::B]))
(def B {:c {:a {:a1 1 :a2 2}}})
(def B2 {:B B})
(s/def ::b3 (s/coll-of ::B2))


(def ast-A (ast/parse ::A))
(def ast-B (ast/parse ::B))

(deftest search-path-test
  (is (= ((matcher/search-path (ast/parse ::B) ::a) B) {:a1 1, :a2 2})))

(deftest keys-matcher-test

  (is (s/conform ::A A))
  (is (s/conform ::B B))

  (is (= identity (matcher/match ast-A ast-A)))

  (is (= :a (matcher/match (ast/parse ::c) (ast/parse ::a))))

  (let [f (matcher/match ast-B (ast/parse ::a))]
    (is (= (f B) {:a1 1 :a2 2})))

  (is (= ((matcher/match (ast/parse ::a) (ast/parse ::c)) {:a1 1 :a2 2})
         {:a {:a1 1, :a2 2}})) 

  (is (= ((matcher/match (ast/parse ::A) (ast/parse ::c)) A) {:a {:a1 1, :a2 2}})) 

  (is (= B ((matcher/match ast-A ast-B) A)))

  (is (= ((matcher/match (ast/parse ::A2) (ast/parse ::B2)) A2) B2)))

(deftest every-matcher-test
  (is (= ((matcher/match (ast/parse ::a3) (ast/parse ::b3)) [A2])
         '({:B {:c {:a {:a1 1, :a2 2}}}}))))

(s/def ::a4 (s/coll-of ::a3))

(def a4 [[{:A A}] [{:A {:a {:a1 4 :a2 5} :b 6}}]])
(def a5 [{:x [{:A A}]} {:x [{:a {:a1 4 :a2 5} :b 6}]}])
((matcher/match (ast/parse ::a4) (ast/parse ::b3)) a4)
