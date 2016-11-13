(ns speculate.transform.spec
  (:require
   [clojure.spec :as s]
   [speculate.spec :as u]))

(s/def ::x integer?)
(s/def ::y integer?)
(s/def ::radius integer?)

(s/def ::point (s/keys :req-un [::x ::y]))


(s/def :test1/square (s/coll-of ::point :min-count 4 :max-count 4))

(s/def :test1/triangle (s/coll-of ::point :min-count 3 :max-count 3))

(s/def :test1/shape #{:square :triangle})

(s/def :test1/shapes
  (u/spec
   :spec (s/keys :req-un [:test1/square :test1/triangle])
   :categorize {:test1/shape keys}))
;; here keys categorizes at a different level than the root map so we
;; should ignore keys categorizations when we're trying to assess
;; categories

(def test1-shapes
  {:square   [{:x 0 :y 0} {:x 50 :y 0} {:x 50 :y 50} {:x 0 :y 50}]
   :triangle [{:x 70 :y 50} {:x 90 :y 0} {:x 110 :y 50}]})


(s/def :test2/type :test1/shape)

(s/def :test2/points (s/coll-of ::point))

(s/def :test2/shape
  (u/spec
   :spec (s/keys :req-un [:test2/type :test2/points])
   :categorize {:test2/type (comp set list :type)}))
;; ^^ this looks to be expressible as a multi-spec

(s/def :test2/shapes (s/coll-of :test2/shape))

(def test2-shapes
  [{:type :square :points [{:x 0 :y 0} {:x 50 :y 0} {:x 50 :y 50} {:x 0 :y 50}]}
   {:type :triangle :points [{:x 70 :y 50} {:x 90 :y 0} {:x 110 :y 50}]}])

(s/def :test3/sub-polygons (s/or :or-type1 :test1/shapes :or-type2 :test2/shapes))

(s/def :test3/polygon (s/keys :req-un [:test3/sub-polygons]))

(s/def :test3/type1 :test3/polygon)
(s/def :test3/type2 :test3/polygon)

(s/def :test3/type #{:type1 :type2})
;; ^^ this must match the keys in :test3/polygons s/keys

(s/def :test3/polygons
  (u/spec
   :spec (s/and (s/keys :opt-un [:test3/type1 :test3/type2])
                #(> (count (keys %)) 0))
   :categorize {:test3/type keys}))

(def test3-polygons
  {:type1 {:sub-polygons test1-shapes}
   :type2 {:sub-polygons test2-shapes}})
;;^^ the alias restriction not working for test3?

;; what about a compressed/dual categorization and round-trip
;; e.g. available-for rate-card

;; test 4 - simple dual categories

(s/def :test4/type #{:a :b :c :d :e})
(s/def :test4/types (s/coll-of :test4/type :kind set? :min-count 2 :max-count 2))
;; ^^ error here must be trying to merge back together?
(s/def :test4/thing1 pos-int?)
(s/def :test4/thing2 pos-int?)
(s/def :test4/simple-map
  (u/spec
   :spec (s/keys :req-un [:test4/thing1 :test4/thing2 :test4/types])
   :categorize {:type :types}))
;; ^^ when there is a key type like above, which is also a key, it
;; becomes hard to reconstruct

;; (s/def :test5/shape
;;   (s/merge :test2/shape (s/keys :opt-un [::radius])))
;; should implement ^^ but hacking for now

(s/def :test5/type #{:square :triangle :circle})

(s/def :test5/shape
  (s/keys :req-un [:test5/type :test2/points]
          :opt-un [::radius]))

(s/def :test5/polygon-set
  (s/coll-of :test5/shape :min-count 1))

(s/def :test5/polygon-type #{:type1 :type2 :type3})

(s/def :test5/polygon-types
  (s/coll-of :test5/polygon-type :kind set? :min-count 1 :max-count 2))
;; what's the condition here?
;; if (f x) = #{:elem-1} and (f y) = #{:elem-2}
;; ∧ x = y ∧ spec f ???? wtf?

(s/def :test5/polygons (s/or :test3 :test3/polygons
                             :test5 :test5/polygon-set))

(s/def :test5/multi-type-polygons
  (u/spec
   :spec (s/keys :req-un [:test5/polygons :test5/polygon-types])
   :categorize {:test5/polygon-type :polygon-types}))

(s/def :test5/polygons-coll
  (s/coll-of :test5/multi-type-polygons))

(def test5-polygons
  (conj test2-shapes
        {:type :circle
         :points [{:x 500 :y 500}]
         :radius 200}))

(def test5-categorized-polygons
  [{:polygon-types #{:type1 :type2}
    :polygons test3-polygons}
   {:polygon-types #{:type3}
    :polygons test5-polygons}])
;; ^^ need a better compress - hack in place to pass atm

(s/def :test6/region
  (s/conformer
   (comp #{10 11 12 13 14 15 16 17 18 19 20 21 22 23}
         #(Integer. (if (keyword? %) (name %) %)))))

(s/def ::10 :test1/shapes)
(s/def ::11 :test1/shapes)
;; ^^ these should be done with map-of, but that's not implemented yet

(s/def :test6/map
  (u/spec
   :spec (s/keys :req-un [::10 ::11])
   :categorize {:test6/region keys}
   ))

(s/def :test7/cat-1 #{"Electricity" "Gas"})
(s/def :test7/cat-2 #{"Standard" "Economy 7"})
(s/def :test7/id pos-int?)

(s/def :test7/meter
  (u/spec
   :spec (s/keys :req-un [:test7/id
                          :test7/cat-1
                          :test7/cat-2
                          :test6/map
                          :test5/polygon-types])
   :categorize {:meter (juxt :cat-1 :cat-2)
                :test5/polygon-type :polygon-types}))

(s/def :test7/meters (s/coll-of :test7/meter :min-count 1 :max-count 2))

(s/def :test8/meters-1 :test7/meters)
(s/def :test8/meters-2 :test7/meters)

(s/def :test8/same-spec-different-values
  (s/keys :req-un [:test8/meters-1 :test8/meters-2]))

(s/def :test9/fuel-type #{"gas" "electricity"})

(s/def :test9/fuel
  (u/spec
   :spec (s/keys :req-un [:test9/fuel-type :test8/same-spec-different-values])
   :categorize {:fuel (comp set list :fuel-type)}))

(s/def :test9/electricity
  (u/spec
   :spec (s/and :test9/fuel #(= "electricity" (:fuel-type %)))
   :select {:fuel #{"electricity"}}))

(s/def :test9/gas
  (u/spec
   :spec (s/and :test9/fuel #(= "gas" (:fuel-type %)))
   :select {:fuel #{"gas"}}))

;; useful if we can generate this from :test9/fuel spec, and filter by
;; categorize/select the below 2nd place and spec does this, so we
;; should be able to move this up, and maybe make it part of the
;; select

(s/def :test9/select-on-key-val
  (s/and (s/keys :opt-un [:test9/electricity :test9/gas])
         #(let [e (:electricity %)
                g (:gas %)]
            (cond (and e g)
                  (and (some-> e :fuel-type #{"electricity"})
                       (some-> g :fuel-type #{"gas"}))
                  e
                  (some-> e :fuel-type #{"electricity"})
                  g
                  (some-> g :fuel-type #{"gas"})))))

(s/def :test10/multiple-test9s
  (s/coll-of :test9/select-on-key-val :min-count 1 :max-count 5))


(s/def :comp/meter (s/coll-of string?))

;;; test 11 redefs
(s/def :test11/meter
  (u/spec
   :spec (s/keys :req-un [:test7/id
                          :test7/cat-1
                          :test7/cat-2
                          :test6/map])
   :categorize {:comp/meter (comp set list (juxt :cat-1 :cat-2))}))

(s/def :test11/meters (s/coll-of :test11/meter :min-count 1 :max-count 2))
(s/def :test11/meters-1 :test11/meters)
(s/def :test11/meters-2 :test11/meters)
(s/def :test11/same-spec-different-values
  (s/keys :req-un [:test11/meters-1 :test11/meters-2]))

(s/def :test11/fuel-type #{"gas" "dual-fuel" "electricity"})
(s/def :test11/fuel
  (u/spec
   :spec (s/keys :req-un [:test11/fuel-type :test11/same-spec-different-values])
   :categorize {:fuel (comp set list :fuel-type)}))

(s/def :test11/electricity
  (u/spec
   :spec :test11/fuel
   :select {:fuel #{"electricity"}}))

;; maybe it will be possible to write select as s/and?
;; (s/and :test11/fuel (comp #{"electricity"} :fuel-type))

(s/def :test11/gas
  (u/spec
   :spec :test11/fuel
   :select {:fuel #{"gas"}}))
;; useful if we can generate this from :test9/fuel spec, and filter by
;; categorize/select the below 2nd place and spec does this, so we
;; should be able to move this up, and maybe make it part of the
;; select

(s/def :test11/select-on-key-val
  (s/and (s/keys :opt-un [:test11/electricity :test11/gas])
         #(let [e (:electricity %)
                g (:gas %)]
            (cond (and e g)
                  (and (some-> e :fuel-type #{"electricity"})
                       (some-> g :fuel-type #{"gas"}))
                  e
                  (some-> e :fuel-type #{"electricity"})
                  g
                  (some-> g :fuel-type #{"gas"})))))
;; need to fix the gen of ^^ cos it sometimes creates multiple of the same categorization

(s/def :test11/extract-shapes
  (s/coll-of :test1/shapes :min-count 1 :max-count 5))
;; ^^ indexing works here

(s/def :test11/shapes (s/coll-of :test1/shapes))

(s/def :test11/new-spec
  (u/spec
   :spec (s/keys :req-un [:test6/region :comp/meter :test7/id
                          :test7/cat-1 :test7/cat-2 :test11/shapes])
   :categorize {;:comp/meter (comp set list :meter)
                ;; :fuel nil
                ;;:test6/region (comp set list :region)
                }))
;; ^^ this test is nonsense

(s/def :test11/new-specs
  (s/coll-of :test11/new-spec))

;; one thing to many smaller things
;; maybe a test9 map to shapes?

(comment

  (s/conform :test1/shapes test1-shapes)
  (s/conform :test2/shapes test2-shapes)
  (s/conform :test3/polygons test3-polygons)
  (s/conform :test5/polygons-coll test5-categorized-polygons)

  )
