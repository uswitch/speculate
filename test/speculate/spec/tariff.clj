(ns speculate.spec.tariff
  (:require
   [clojure.spec.alpha :as s]
   [speculate.spec :as u]
   [speculate.util :as util]
   [speculate.spec.rate-card :as rate-card]))

(s/def ::fuel-type ::rate-card/fuel)

(s/def :tariff.rate/type
  (u/spec
   :spec #{"unit-rate" "night-rate" "tiered-rate"}
   :alias {::rate-card/rate #(cond (:night-rate %) "night-rate"
                                   (:threshold %) "tiered-rate"
                                   :default "unit-rate")}))

(s/def ::rate
  (s/keys :req-un [:tariff.rate/type
                   ::rate-card/price
                   ::rate-card/threshold]))

(s/def ::rates
  (s/coll-of ::rate))

(s/def ::standing-charge ::rate-card/annual-standing-charge)

(s/def ::fuel
  (s/keys :req-un [::fuel-type ::rate-card/variant ::standing-charge ::rates]))

(s/def ::electricity
  (u/spec
   :spec ::fuel
   :select {:meter #{["Electricity" "Standard"] ["Electricity" "Economy 7"]}
            :fuel #{"electricity"}
            ::rate-card/available-for #{"dual-fuel" "electricity"}}))

(s/def ::gas
  (u/spec
   :spec ::fuel
   :select {:meter #{["Gas" "Standard"]}
            :fuel #{"gas"}
            ::rate-card/available-for #{"dual-fuel" "gas"}}))

(s/def ::period ::rate-card/duration)

(s/def ::date ::rate-card/end-date)

(s/def :tariff-ends/type
  (u/spec
   :spec #{"period" "date"}
   :alias {::rate-card/duration #(if % "period" "date")}))

(s/def ::tariff-ends
  (s/keys :req-un [:tariff-ends/type] :opt-un [::period ::date]))

(s/def ::price-guarantee
  (s/keys :req-un [:tariff-ends/type] :opt-un [::period ::date]))

(s/def ::tariff-id ::rate-card/uuid)

(s/def ::tariff-type
  (u/spec
   :spec #{"fixed" "variable"}
   :alias {::rate-card/fixed #(if % "fixed" "variable")}))

(s/def ::id ::rate-card/supplier-name-key)
(s/def ::name ::rate-card/supplier-name)
(s/def ::supplier
  (s/keys :req-un [::id ::name])) 

(s/def ::variant
  (u/spec
    :spec ::rate-card/variant
    :select {:fuel #{"electricity"}}))

(defn restrict-availability
  [{categorize :categorize}]
  (let [{:keys [::rate-card/available-for ::rate-card/fuel]} categorize]
    (or (= available-for "dual-fuel")
        (= available-for fuel)
        (= available-for (:fuel categorize)))))

(s/def ::tariff
  (u/spec
   :spec (s/keys :req-un [::tariff-id
                          ::rate-card/name
                          ::rate-card/payment-method
                          ::tariff-type 
                          ::rate-card/supplier-name-key
                          ::supplier
                          ::rate-card/region
                          ::variant
                          ::rate-card/available-for]
                 :opt-un [::electricity
                          ::gas
                          ::tariff-ends
                          ::price-guarantee])
   :categorize {::rate-card/payment-method nil
                ::rate-card/region nil
                :electricity.variant nil
                ::rate-card/available-for nil}
   :select {::rate-card/available-for restrict-availability}))

(s/def ::tariffs (s/coll-of ::tariff))
