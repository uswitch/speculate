(ns speculate.transform-test
  (:require
   [clojure.spec :as s]
   [clojure.spec.gen :as gen]
   [clojure.spec.test :as test]
   [clojure.test :refer [deftest is]]
   [speculate.parse :as p]
   [speculate.transform :as t]
   [speculate.transform.combine :as tc]
   [speculate.transform.extract :as tx]
   [speculate.transform.spec :as ts]
   [clojure.set :as set]
   [clojure.java.io :as io]))

(def pp clojure.pprint/pprint)

(defn extract [spec value]
  (tx/run-walk (p/ast spec) value #{}))

(defn gen-extract [spec]
  (let [gen-value (gen/generate (s/gen spec))]
    (pp gen-value)
    (extract spec gen-value)))

(defn round-trip [spec value]
  (let [ast (p/ast spec)
        [value-index state] (tx/run-walk ast value #{})]
    (tc/-combine value-index (:categorized state) ast)))

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

(defn transform [from-spec to-spec value]
  (let [[value-index state] (time (extract from-spec value))]
    (time (tc/-combine value-index (:categorized state) (p/ast to-spec)))))

(defn gen-transform [from-spec to-spec]
  (let [[value-index state] (gen-extract from-spec)]
    (tc/-combine value-index (:categorized state) (p/ast to-spec))))

(deftest shape-test1
  (is (= (round-trip :test1/shapes ts/test1-shapes)
         ts/test1-shapes)))

(deftest shape-test2
  (is (= (round-trip :test2/shapes ts/test2-shapes)
         ts/test2-shapes)))

(deftest polygons-test3
  (is (= (round-trip :test3/polygons ts/test3-polygons)
         ts/test3-polygons)))

(deftest things-test4
  (is (apply = (gen-round-trip :test4/simple-map))))

(deftest polygons-test5
  (is (= (round-trip :test5/polygons-coll ts/test5-categorized-polygons)
         ts/test5-categorized-polygons)))

(deftest region-keys-test6
  (is (apply = (gen-round-trip :test6/map))))

(deftest test7
  (is (exercise-round-trip :test7/meters)))

(deftest test8
  (is (exercise-round-trip :test8/same-spec-different-values)))

(deftest test9
  (is (exercise-round-trip :test9/select-on-key-val)))

(deftest test10
  (is (exercise-round-trip :test10/multiple-test9s)))

(defn read-resource [filename]
  (read-string (slurp (io/resource filename))))

(deftest test11
  (let [[test-data result] (read-resource "test11.edn")]
    (is (= (transform :test11/select-on-key-val :test11/new-specs test-data)
           result))))

(deftest test11b
  (let [categorize   (read-resource "test11.categorize.edn")
        categorized  (read-resource "test11.categorized.edn")
        value-index  (read-resource "test11.value-index.edn")
        value-index' (read-resource "test11.value-index-prime.edn")]
    (is (= (tc/categorize categorized categorize value-index)
           value-index'))))

;; (let [[vi state] (read-resource "test3.extract.edn")
;;       expanded   (tc/expand-value-index (:categorized state) vi)]
;;   (= vi (tc/compress expanded)))

;; (tc/categorize (read-resource "test11.categorized.edn") (read-resource "test11.categorize.edn") (read-resource "test11.value-index.edn"))

;; (deftest test11
;;   (dotimes [_ 20]
;;     (let [spec :test11/select-on-key-val
;;           val (gen/generate (s/gen spec))
;;           shape-list (fn [k x]
;;                        (->> (k x)
;;                             (:same-spec-different-values)
;;                             (vals)
;;                             (apply concat)
;;                             (mapcat (comp vals :map))))
;;           shapes (concat (shape-list :electricity val)
;;                          (shape-list :gas val))
;;           [value-index state] (extract spec val)
;;           extracted (tc/-combine value-index
;;                                  (:categorized state)
;;                                  (p/ast :test11/extract-shapes))]
;;       #_(when (not= (set shapes) (set extracted))
;;         (when (not= (count shapes) (count extracted))
;;           (prn 'shapes (count shapes))
;;           (prn 'extracted (count extracted))
;;           (pp val)
;;           (pp extracted)
;;           ;; (pp (set/difference (set shapes) (set extracted)))
;;           ;; (pp (set/difference (set extracted) (set shapes)))
;;           ))
;;       (is (= (set shapes) (set extracted)))
;;       )))
;; ^^ this test is not good for transform, but it may be good for find
;; (shape-test2)

;; round-trip testing cannot test aliases

;; what if categorizations are the same...?
