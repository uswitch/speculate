(ns speculate.transform-test
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as test]
   [clojure.test :refer [deftest is]]
   [speculate.ast :as ast]
   [speculate.transform :as t]
   [speculate.transform.combine :as tc]
   [speculate.transform.extract :as tx]
   [speculate.transform.spec :as ts]
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def pp clojure.pprint/pprint)

(defn extract [spec value]
  (tx/run-walk (ast/parse spec) value #{}))

(defn gen-extract [spec]
  (let [gen-value (gen/generate (s/gen spec))]
    (extract spec gen-value)))

(defn round-trip [spec value]
  (t/transform spec spec value))

(defn gen-round-trip [spec]
  (let [gen-value (gen/generate (s/gen spec))]
    [gen-value (round-trip spec gen-value)]))

(defn exercise-round-trip
  ([n spec]
   (->> (repeat n spec)
        (map (comp (partial apply =) gen-round-trip))
        (every? true?)))
  ([spec]
   (exercise-round-trip 10 spec)))

;; (deftest test1
;;   (is (= (round-trip :test1/shapes ts/test1-shapes)
;;          ts/test1-shapes)))

;; (deftest test2
;;   (is (= (round-trip :test2/shapes ts/test2-shapes)
;;          ts/test2-shapes)))

;; (deftest test3-a
;;   (is (exercise-round-trip :test3/type1)))

;; (deftest test3-b
;;   (is (exercise-round-trip :test3/sub-polygons)))

;; (deftest test3
;;   (is (= (round-trip :test3/polygons ts/test3-polygons)
;;          ts/test3-polygons)))

;; (deftest test4
;;   (is (apply = (gen-round-trip :test4/simple-map))))

;; (deftest test5
;;   (is (= (round-trip :test5/polygons-coll ts/test5-categorized-polygons)
;;          ts/test5-categorized-polygons)))

;; (deftest test6-a
;;   (is (exercise-round-trip (keyword "speculate.transform.spec" "10"))))

;; (deftest test6
;;   (is (apply = (gen-round-trip :test6/map))))

;; (deftest test7
;;   (is (exercise-round-trip :test7/meters)))

;; (deftest test8
;;   (is (exercise-round-trip :test8/same-spec-different-values)))

;; (deftest test9
;;   (is (exercise-round-trip :test9/select-on-key-val)))

;; (deftest test10
;;   (is (exercise-round-trip :test10/multiple-test9s)))

;; (defn read-resource [filename]
;;   (read-string (slurp (io/resource filename))))
