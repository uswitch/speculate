(ns speculate.ensure-req-key-present-in-target-test
  (:require
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest is]]
    [speculate.transform :as tx]))

(s/def ::a (s/nilable int?))
(s/def ::b (s/nilable int?))

(s/def ::from (s/keys :opt-un [::a ::b]))
(s/def ::to   (s/keys :req-un [::a ::b]))

(deftest make-sure-req-key-present-in-target-when-missing-in-source
  (let [from {:a nil}]
    (is (s/valid? ::from from))
    (is (= {:a nil :b nil} 
           (tx/transform ::from ::to from))))

  (let [from {:a 2}]
    (is (s/valid? ::from from))
    (is (= {:a 2 :b nil} 
           (tx/transform ::from ::to from))))

  (let [from {}]
    (is (s/valid? ::from from))
    (is (= {:a nil :b nil} 
           (tx/transform ::from ::to from)))))

(comment
  (make-sure-req-key-present-in-target-when-missing-in-source))
