(ns speculate.rate-card-tariff-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is]]
   [speculate.spec.rate-card :as rate-card]
   [speculate.spec.tariff :as tariff]
   [speculate.transform :as tx]))

(defn k= [k & [k2]]
  (let [fields {:rate-guarantee {:end-date nil
                                 :fixed true
                                 :duration 24
                                 :available 24}
                :name "Energy Supplier Energy Plan 1"
                :name-key "energy-supplier-energy-plan-1"
                :supplier-name "Energy Supplier Name"
                :supplier-name-key "energy-supplier-name-key"
                :supplier {:id "energy-supplier-name-key", :name "Energy Supplier Name"}
                :status "published"
                :uuid #uuid "29e5d2e5-e073-4a58-9c33-6c358bc7e1e1"}
        k2 (or k2 k)]
    (fn [val]
      (= (k fields) (k2 val)))))

(defn matches-fuels? [{:keys [available-for electricity gas]}]
  (case available-for
    "electricity" (and electricity (not gas))
    "gas"         (and gas (not electricity))
    "dual-fuel"   (and electricity gas)))

(defn matches-region-price? [{:keys [region] :as tariff}]
  (letfn [(matches? [{:keys [standing-charge rates]}]
            (let [[unit-rate]  (filter (comp #{"unit-rate"} :type) rates)
                  [night-rate] (filter (comp #{"night-rate"} :type) rates)]
              (and (= region (int (/ standing-charge 100)))
                   (= region (int (/ (:price unit-rate) 1000)))
                   (if night-rate
                     (= region (int (/ (:price night-rate) 1000)))
                     true))))]
    (and (if-let [e (:electricity tariff)] (matches? e) true)
         (if-let [e (:gas tariff)] (matches? e) true))))

(defn matches-unit-night-rate-price? [tariff]
  (letfn [(matches? [{:keys [rates]}]
            (let [[unit-rate]  (filter (comp #{"unit-rate"} :type) rates)
                  [night-rate] (filter (comp #{"night-rate"} :type) rates)]
              (and (zero? (int (mod (:price unit-rate) 10)))
                   (if night-rate
                     (= 1 (int (mod (:price night-rate) 10)))
                     true))))]
    (and (if-let [e (:electricity tariff)] (matches? e) true)
         (if-let [e (:gas tariff)] (matches? e) true))))

(def fuel-payment-method-index
  {"Monthly Direct Debit"   {"Gas"         {"Standard"  0}
                             "Electricity" {"Economy 7" 1 
                                            "Standard"  2}}
   "Variable Direct Debit"  {"Gas"         {"Standard"  0}
                             "Electricity" {"Economy 7" 1  
                                            "Standard"  2}}
   "Pay on Receipt of Bill" {"Gas"         {"Standard"  3}
                             "Electricity" {"Economy 7" 4
                                            "Standard"  5}}})

(defn matches-fuel-payment-method-price? [{:keys [payment-method] :as tariff}]
  (letfn [(matches? [fuel {:keys [standing-charge rates variant]}]
            (let [[unit-rate]  (filter (comp #{"unit-rate"} :type) rates)
                  [night-rate] (filter (comp #{"night-rate"} :type) rates)
                  index (get-in fuel-payment-method-index
                                [payment-method fuel variant])]
              (and (= index (mod (int (/ standing-charge 10)) 10))
                   (= index (mod (int (/ (:price unit-rate) 100)) 10))
                   (if night-rate
                     (= index (mod (int (/ (:price night-rate) 100)) 10))
                     true))))]
    (and (if-let [e (:electricity tariff)] (matches? "Electricity" e) true)
         (if-let [e (:gas tariff)] (matches? "Gas" e) true))))

(deftest rate-card-tx-tariffs-test
  (let [simple-rate-card (-> (slurp "test-resources/rate-card.edn")
                             (edn/read-string))
        tariffs (tx/transform ::rate-card/rate-card
                              ::tariff/tariffs
                              simple-rate-card)]
    (is (every? (k= :uuid :tariff-id) tariffs))
    (is (every? (k= :name) tariffs))
    (is (every? (k= :supplier-name-key) tariffs))
    (is (every? (k= :supplier) tariffs))
    (is (every? matches-fuels? tariffs))
    (is (every? matches-region-price? tariffs))
    (is (every? matches-unit-night-rate-price? tariffs))
    (is (every? matches-fuel-payment-method-price? tariffs))))
