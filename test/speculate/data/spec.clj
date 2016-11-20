(ns speculate.data.spec
  (:require
   [clojure.spec :as s]
   [speculate.spec :as u]
   [speculate.transform :as t]))

(def square-1
  {:type        :square
   :id          :sq-1
   :coordinates [{:type :point :x 20 :y 30}
                 {:type :point :x 50 :y 30}
                 {:type :point :x 50 :y 60}
                 {:type :point :x 20 :y 60}]})

(def coordinate-vector-1
  {:type        :coordinate-vector
   :id          :cv-1
   :x-coords [20 50 50 20]
   :y-coords [30 30 60 60]})

(s/def ::x int?)
(s/def ::y int?)
(s/def ::type keyword?)
(s/def ::point (u/spec :spec (s/keys :req-un [::type ::x ::y])))
(s/def ::coordinates (s/coll-of ::point :kind vector?))
(s/def ::id keyword?)
(s/def ::square (s/keys :req-un [::type ::id ::coordinates]))

(s/def ::x-coords (s/coll-of ::x :kind vector?))
(s/def ::y-coords (s/coll-of ::y :kind vector?))
(s/def ::coordinate-vector (s/keys :req-un [::type ::id ::x-coords ::y-coords]))

(defn isomorphic? [spec-a spec-b value]
  (->> value
       (t/transform spec-a spec-b)
       ;; (t/transform spec-b spec-a)
;;       (= value)
       ))

;; (isomorphic? ::square ::coordinate-vector square-1)
(t/transform ::coordinate-vector ::square coordinate-vector-1)
