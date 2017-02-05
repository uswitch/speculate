(ns speculate.lens-test
  (:refer-clojure :exclude [set])
  (:require
   [bifocal.lens :as bi :refer [view over set]]
   [clojure.spec :as s]
   [clojure.test :refer [deftest is]]
   [speculate.ast :as ast]
   [speculate.lens :as lens]
   [speculate.spec :as u]))

(s/def ::a string?)
(s/def ::b int?)
(s/def ::c keyword?)

(s/def ::d #{"test"})

(s/def ::x (s/keys :req-un [::a ::b ::c]))
(s/def ::y (s/keys :req-un [::d]))

(s/def ::z (s/or :x ::x :y ::y))
(s/def ::z2 (s/and ::z map?))

(deftest mk-keys-test
  (let [x {:a "test" :b 10 :c :thing}
        l (lens/mk (ast/parse ::x))]
    (is (= x (set l nil (view l x))))))

(deftest mk-or-test
  (is (= (view (lens/mk (ast/parse ::z)) {:a "test" :b 10 :c :thing})
      ["test" 10 :thing]))
  (is (= (view (lens/mk (ast/parse ::z)) {:d "test"})
         ["test"])))

(deftest mk-and-test
  (is (= (view (lens/mk (ast/parse (s/and ::z map?)))
               {:a "test" :b 10 :c :thing})
         ["test" 10 :thing])))

(deftest mk-every-test
  (let [x {:a "test" :b 10 :c :thing}
        s (s/coll-of ::z2)
        l (lens/mk (ast/parse s))]
    (is (= (view l [x x x])
           '(["test" 10 :thing] ["test" 10 :thing] ["test" 10 :thing])))))

(deftest mk-nilable-test
  (let [x {:a "test" :b 10 :c :thing}
        s (s/nilable (s/coll-of ::z2))
        l (lens/mk (ast/parse s))]
    (is (= (view l [x x x])
           '(["test" 10 :thing] ["test" 10 :thing] ["test" 10 :thing])))
    (is (nil? (view l nil)))))

(s/def ::arg-list (s/cat :a ::a :b ::b :c (s/coll-of ::z)))

;; (view (lens/mk (ast/parse (s/form ::arg-list))) '("test" 10 [:thing]))

;; (ast/conform-1 (ast/parse (s/cat :a (s/? int?) :b string?)) [1 "test"])

;; (require 'clojure.spec.override)

;; (let [x ["test" 10 :thing]
;;       s (s/cat :a ::a :b ::b :c ::c)
;;       p (ast/parse s)
;;       c (ast/conform-1 p x)
;;       ]
;;   c)

;; Limitations of cat as a transform target are that regex ops are
;; pretty useless to target. They are not specific, so cannot be
;; targetted properly. They are just patterns.
;; But using cat to define positional targets would work
;; Maybe tuple would be better? That is more specific.

(set (lens/mk (ast/parse (s/tuple ::x)))
     nil
     [["test" 10 :thing]])

;; (s/tuple ::a ::b ::c)

;; (s/conform (s/? int? ) ["set"])

;; (ast/unparse (ast/parse (s/cat :a (s/? int?) :b string?)))

;; ["test"]

;; (ast/conform-1 (ast/parse ::arg-list) '("test" 10 [{:d "test"}]))
;; (ast/conform-1 (ast/parse ::arg-list) [{:d "test"}])
(s/def ::value int?)
;; (bi/value {:type "type-1"} 5)
;; (ast/parse (u/select string? :type #{"type-1"}))


(deftest categorize-test
  (let [spec (u/categorize (s/keys :req-un [::value]) :type :type)]
    (= (view (lens/mk (ast/parse spec))
             {:type "type-1" :value 5})
       (bi/value {:type "type-1"} 5))))

(let [spec (u/select (u/categorize (s/keys :req-un [::value]) :type :type)
                     :type #{"type-1"})
      lens (lens/mk (ast/parse spec))]
  (view lens
        [{:type "type-1" :value 5} {:type "type-2" :value 4}])
  )

;; {:speculate.ast/type speculate.spec/categorize
;;  :form {:speculate.ast/type clojure.core/symbol?
;;         :form clojure.core/map?}
;;  :categorize {:type :type}}

;; (clojure.test/run-tests)
