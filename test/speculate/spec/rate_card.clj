(ns speculate.spec.rate-card
  (:require
   [clj-time.coerce :refer [to-date]]
   [clj-time.format :refer [formatter parse]]
   [clojure.spec :as s]
   [speculate.spec :as u]
   [speculate.util :as util]))

(def dfmt (formatter "dd/MM/yyyy"))
(defn coerce [pred coerce-fn]
  (s/conformer (comp #(when (pred %) %)
                     #(try (coerce-fn %) (catch Throwable _ %)))
               identity))

(def coerce-date?
  (coerce #(instance? java.util.Date %) #(to-date (parse dfmt %))))

(s/def ::region
  (coerce #{10 11 12 13 14 15 16 17 18 19 20 21 22 23}
          #(Integer. (if (keyword? %) (name %) %))))

(def coerce-uuid?
  (coerce uuid? #(if (string? %) (java.util.UUID/fromString %) %)))

(s/def ::uuid coerce-uuid?)

;;; ::tariff

(s/def ::payment-method
  #{"Monthly Direct Debit"
    "Variable Direct Debit"
    "Pay On Receipt Of Bill"
    "Pay on Receipt of Bill"
    "Prepayment"})

(s/def ::payment-methods (s/coll-of ::payment-method))

(s/def ::variant #{"Standard" "Economy 7"})

(s/def ::meter (s/keys :req-un [::fuel ::variant]))

(s/def ::tariff
  (u/spec
   :spec (s/keys :req-un [::regional-rates ::meter ::payment-methods])
   :categorize {:meter (comp set list (juxt :fuel :variant) :meter)
                :fuel  (comp set list util/hyphenate :fuel :meter)
                :electricity.variant (fn [{{:keys [fuel variant]} :meter}]
                                       (when (= fuel "Electricity") #{variant}))
                ::payment-method (comp set :payment-methods)}))

(s/def ::tariffs (s/coll-of ::tariff))

(s/def ::night-rate boolean?)

(s/def ::threshold (s/nilable nat-int?))

(def coerce-decimal? (coerce decimal? bigdec))

(s/def ::price coerce-decimal?)

(s/def ::rate (s/keys :req-un [::night-rate ::price ::threshold]))

(s/def ::rates (s/coll-of ::rate :max-count 3))

(s/def ::annual-standing-charge (s/nilable coerce-decimal?))

(s/def ::rate-guaranteed-until
  (s/nilable (s/or :date coerce-date? :months nat-int?)))

(s/def ::fuel
  (coerce #{"electricity" "gas"} util/hyphenate))

(s/def ::supplier-name-key string?)
(s/def ::supplier-name string?)
(s/def ::supplier string?)
(s/def ::name-key string?)
(s/def ::name string?)

(s/def ::regional-rate (s/keys :req-un [::annual-standing-charge ::rates]))

(s/def ::11 ::regional-rate)
(s/def ::12 ::regional-rate)
(s/def ::14 ::regional-rate)
(s/def ::18 ::regional-rate)

(s/def ::regional-rates
  (u/spec
   :spec (s/keys :opt-un [::11 ::12 ::14 ::18])
   :categorize {::region keys}))

(s/def ::status #{"draft" "published" "historical" "archived"})

(s/def ::end-date coerce-date?)
(s/def ::duration (s/nilable nat-int?))
(s/def ::available (s/nilable (s/or :date coerce-date? :months nat-int?)))
(s/def ::fixed (s/or :date coerce-date? :bool boolean?))
(s/def ::rate-guarantee
  (s/keys :req-un [::end-date ::duration ::fixed ::available]))

(s/def ::unfulfillable-payment-methods vector?)

(s/def ::available-until (s/nilable nat-int?))

(s/def ::available-for
  (coerce #{"dual-fuel" "electricity" "gas"} util/hyphenate))

(s/def ::availability (s/coll-of ::available-for))

(s/def ::rate-card
  (u/spec
   :spec (s/keys :req-un [::uuid
                          ::tariffs
                          ::status
                          ::supplier-name
                          ::supplier-name-key
                          ::name-key
                          ::name
                          ::rate-guarantee
                          ::unfulfillable-payment-methods
                          ::availability]
                 :opt-un [::available-until])
   :categorize {::available-for (fn [{:keys [availability]}]
                                  (->> availability
                                       (map #(s/conform ::available-for %))
                                       (remove #{::s/invalid})
                                       (set)))}))
