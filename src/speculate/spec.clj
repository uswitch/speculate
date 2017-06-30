(ns speculate.spec
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [clojure.walk :as walk]
   [speculate.util :as util]))

(alias 'c 'clojure.core)

(defn ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (if (var? x)
    (let [^clojure.lang.Var v x]
      (symbol (str (.name (.ns v)))
              (str (.sym v))))
    x))

(defn ->gen [x]
  (when x (if (fn? x) x #(-> x))))

(defn ->spec [ns x]
  (cond (string? x) (keyword ns x)
        (keyword? x) (keyword ns (name x))
        :else (throw (Exception. (format "Cannot coerce %s to a spec"
                                         (pr-str x))))))
(defmacro set-of [pred & opts]
  `(s/every ~pred ::conform-all true :kind set? ~@opts))

(defn parse-min-max [maximum exclusive-maximum minimum exclusive-minimum]
  (cond (and minimum maximum)
        (cond (and exclusive-minimum exclusive-maximum)
              [(s/and integer? #(> % minimum) #(< % maximum))
               (gen/choose (inc minimum) (dec maximum))]
              exclusive-minimum
              [(s/and integer? #(> % minimum) #(<= % maximum))
               (gen/choose (inc minimum) maximum)]
              exclusive-maximum
              [(s/and integer? #(>= % minimum) #(< % maximum))
               (gen/choose minimum (dec maximum))]
              :else
              [(s/and integer? #(>= % minimum) #(<= % maximum))
               (gen/choose minimum maximum)])
        minimum
        (if exclusive-minimum
          [(s/and integer? #(> % minimum))
           (gen/choose (inc minimum) Integer/MAX_VALUE)]
          [(s/and integer? #(>= % minimum))
           (gen/choose minimum Integer/MAX_VALUE)])
        maximum
        (if exclusive-maximum
          [(s/and integer? #(< % maximum))
           (gen/choose Integer/MIN_VALUE (dec maximum))]
          [(s/and integer? #(<= % maximum))
           (gen/choose Integer/MIN_VALUE maximum)])))

(defn build-spec [gen spec tests forms]
  (let [c (count tests)]
    (cond (and (zero? c) spec)
          (s/spec-impl (:spec forms) spec (->gen gen) nil)
          spec
          (s/and-spec-impl (apply vector (:spec forms) (c/map s/form tests))
                           (apply vector spec tests)
                           (->gen gen))
          (= c 1)
          (s/spec-impl (s/form (first tests)) (first tests) (->gen gen) nil)
          (> c 1)
          (s/and-spec-impl (mapv s/form tests)
                           (vec tests)
                           (->gen gen)))))

(defn parse-spec
  "Look at a fixed set of expected params and default them where necessary.
  Returns a generate, a plain clojure spec (possibly and and of all the tests),
  and all the tests."
  [{:keys [maximum exclusive-maximum
           minimum exclusive-minimum
           multiple-of
           max-length
           min-length
           pattern
           max-items min-items
           unique-items
           max-properties
           min-properties
           required
           spec
           gen] :as definition}]
  (let [[min-max-test min-max-gen] (parse-min-max maximum exclusive-maximum
                                                  minimum exclusive-minimum)
        multiple-of-test  (when multiple-of
                            (s/spec #(and (not (zero? %))
                                          (integer? (/ % multiple-of)))))
        max-length-test   (when max-length
                            (s/spec #(<= (count %) max-length)))
        min-length-test   (when min-length
                            (s/spec #(>= (count %) min-length)))
        pattern-test      (when pattern
                            (s/spec #(re-matches pattern %)))
        max-items-test    (when max-items
                            (s/spec #(<= (count %) max-items)))
        min-items-test    (when min-items
                            (s/spec #(>= (count %) min-items)))
        unique-items-test (when unique-items
                            (s/spec #(= % (distinct %))))
        tests (remove nil?
                      [min-max-test multiple-of-test max-length-test
                       min-length-test pattern-test max-items-test
                       min-items-test unique-items-test])]
    [(or gen min-max-gen) spec tests]))

(defn postwalk-escape [f escape? form]
  (if (escape? form)
    form
    (walk/walk (partial postwalk-escape f escape?) f form)))

(defn syntax-quote-symbol [form]
  (cond (symbol? form)
        (if-let [s (resolve form)]
          (list 'quote (->sym s))
          form)
        (list? form)
        (apply list 'list form)
        :default form))

(defmacro override [base-spec & impls]
  (let [impls-spec (s/cat :spec-overrides (s/* seq?)
                          :protocol-defs  (s/* (s/cat :sym symbol?
                                                      :list (s/* list?))))
        impls' (s/conform impls-spec impls)
        {:keys [spec-overrides protocol-defs]} impls']
    `(reify
       s/Spec
       ~@(c/map #(or (get (->> spec-overrides
                               (c/map (juxt (comp util/res first) identity))
                               (into {}))
                          (first %))
                     `(~@%))
           `[(s/conform*  [_# x#] (s/conform ~base-spec x#))
             (s/unform*   [_# x#] (s/unform ~base-spec x#))
             (s/explain*  [_# path# via# in# x#] (s/explain* ~base-spec path# via# in# x#))
             (s/gen*      [_# overrides# path# rmap#] (s/gen ~base-spec))
             (s/with-gen* [_# gfn#] (s/with-gen ~base-spec gfn#))
             (s/describe* [_#] (s/describe (s/spec ~base-spec)))])
       ~@(c/mapcat (fn [pdef]
                     (concat [(:sym pdef)]
                             (:list pdef)))
           protocol-defs))))

(defn spec-impl
  "Build an object which conforms to the s/Spec protocol, utilising the core
  functions."
  [gen spec tests categorize select alias form spec-form]
  (let [spec' (build-spec gen spec tests form)
        dt    (partial s/conform spec')
        opts  (apply concat form)]
    (reify
      s/Spec
      (s/conform* [_ x] (dt x))
      (s/unform* [_ x]
        (if spec
          (try
            (s/unform* spec x)
            (catch IllegalArgumentException _ x))
          x))
      (s/explain* [_ path via in x]
        (cond (s/spec? spec)
              (s/explain* spec path via in x)
              (keyword? spec)
              (s/explain* ((s/registry) spec) path via in x)
              :form
              [{:path path :pred (s/abbrev spec-form) :val x :via via :in in}]))
      (s/gen* [_ _ _ _]
        (if gen (if (fn? gen) (gen) gen) (s/gen spec')))
      (s/with-gen* [_ gfn] (spec-impl gfn spec tests form))
      (s/describe* [_] `(spec ~@opts))

      clojure.lang.IDeref
      (deref [_] {:categorize categorize :select select :alias alias}))))

(defmacro spec
  [& {:keys [spec gen categorize select alias] :as definition}]
  `(let [[gen# spec# tests#] (parse-spec ~definition)]
     (spec-impl gen#
                spec#
                tests#
                ~categorize
                ~select
                ~alias
                ~(postwalk-escape
                  syntax-quote-symbol
                  (fn [form]
                    (and (seq? form) (= 'fn* (first form))))
                  (dissoc definition :alias :categorize :select :gen))
                '~(util/res spec))))

(defn strict-impl [keys-spec keys-form form]
  (reify
    s/Spec
    (s/conform* [_ x] (s/conform* keys-spec x))
    (s/unform* [_ x]  (s/unform keys-spec x))
    (s/explain* [_ path via in x]
      (when-not (s/valid? keys-spec x)
        (concat (s/explain* keys-spec path via in x)
                [{:path path
                  :pred (list 'strict-extra-keys
                              (set/difference (set (keys x))
                                              (set (map util/un-ns (:req-un keys-form)))
                                              (set (map util/un-ns (:opt-un keys-form)))))
                  :val x
                  :via via
                  :in in}])))
    (s/gen* [_ overrides path rmap] (s/gen* keys-spec overrides path rmap))
    (s/with-gen* [_ gfn] (s/with-gen* keys-spec gfn))
    (s/describe* [_] (list `strict form))))

(defmacro strict
  "Takes `clojure.spec/keys` forms, and/or ::keywords that resolve to
 `clojure.spec.keys` specs. Merges them and wraps them in a \"strictly
 only these keys\" context."
  [& keys-forms]
  (let [{:keys [req req-un opt opt-un] :as keys-form}
        (->> keys-forms
             (map #(->> (if (keyword? %) (s/form %) %)
                        (rest)
                        (apply hash-map)))
             (apply merge-with (comp vec concat)))
        keys-forms (util/res keys-forms)]
    `(strict-impl (s/and (s/keys ~@(apply concat keys-form))
                         #(= (set (map util/un-ns ~req-un))
                             (set/difference (set (keys %))
                                             ~(set (map util/un-ns opt-un)))))
                  ~keys-form
                  (list `s/keys ~@(apply concat keys-form)))))
