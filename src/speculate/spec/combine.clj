(ns speculate.spec.combine
  (:require
   [clojure.spec.alpha :as s]
   [clojure.future :refer :all]))

(s/def ::label keyword?)
(s/def ::value any?)
(s/def ::category (s/nilable (s/map-of keyword? (s/nilable any?))))
(s/def ::categorize (s/nilable (s/map-of keyword? (s/nilable set?))))
(s/def ::coll-indexes (s/nilable (s/map-of keyword? nat-int?)))
(s/def ::index-value
  (s/keys :req-un [::label ::value]
          :opt-un [::categorize ::coll-indexes]))
(s/def ::value-index (s/coll-of ::index-value))

(s/def ::categorized ::categorize)
(s/def ::pathset-union set?)
(s/def ::from-nodeset set?)
(s/def ::index-meta
  (s/keys :req-un [::categorized ::pathset-union ::from-nodeset]))

(s/fdef combine
  :args (s/cat :value-index ::value-index :index-meta any? :ast any?)
  :ret any?)

(s/fdef coll-combine
  :args (s/cat :value-index ::value-index :index-meta any? :ast any?)
  :ret any?)

(s/fdef expand-value-index
  :args (s/cat :categorized ::categorized)
  :ret  (s/coll-of ::category))
