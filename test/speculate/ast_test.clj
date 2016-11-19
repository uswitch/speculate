(ns speculate.ast-test
  (:require
   [clojure.spec :as s]
   [clojure.test :as t :refer [deftest is]]
   [speculate.ast :as ast]
   [speculate.transform.spec]
   [speculate.walk :as walk]))

(deftest test1
  (let [a (ast/parse :test1/shapes)]
    (is (= a (ast/shake (walk/nodeset a) a)))))

(deftest test2
  (let [a (ast/parse :test2/shapes)]
    (is (= a (ast/shake (walk/nodeset a) a)))))

(deftest test3
  (let [a (ast/parse :test3/polygons)
        b (ast/parse :test2/shapes)]
    (is (= a (ast/shake (walk/nodeset b) a)))))

(s/def ::x int?)
(s/def ::y int?)
(s/def ::test4-a (s/keys :req-un [::x ::y]))
(s/def ::test4-b (s/keys :req-un [::x]))

(deftest test4
  (let [a (ast/parse ::test4-a)
        b (ast/parse ::test4-b)]
    (is (= (ast/shake (walk/nodeset b) a)
           '{:speculate.ast/type clojure.spec/keys,
             :form
             {:req-un
              ({:speculate.ast/type clojure.core/symbol?,
                :form clojure.core/int?,
                :speculate.ast/name :speculate.ast-test/x,
                :leaf true})},
             :speculate.ast/name :speculate.ast-test/test4-a}))))
