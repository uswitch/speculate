(ns speculate.swagger
  (:refer-clojure :exclude [derive find])
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [speculate.ast :as ast]
   [speculate.render :as render]
   [speculate.json-schema :as json]
   [speculate.spec :as spec]
   [speculate.util :as util]))

(defn find
  "Performs a depth-first search in `x` for key `k`, returns `val` for
  the first found `k`."
  [x k]
  (if (and (associative? x) (contains? x k))
    (x k)
    (cond (map? x)
          (some #(find % k) (vals x))
          (sequential? x)
          (some #(find % k) x))))

(defn match
  "Performs a depth-first search in `x` for key `k`, where `val`
  matches predicate function pred. returns `val` for the first matched
  `k` -> `(pred val)`."
  [x pred & preds]
  (or (when (pred x)
        (if (seq preds)
          (apply match x preds)
          x))
      (cond (map? x)
            (some #(apply match % pred preds) (vals x))
            (sequential? x)
            (some #(apply match % pred preds) x))))


;;; Responses

(defn response [{:keys [form description] :as response}]
  [(render/render ::render/abbrev (find form :status))
   {:description (render/render ::json/renderer description)
    :schema (render/render ::json/renderer (find form :body))}])

(def -responses nil)
(defmulti -responses ::ast/type)

(defmethod -responses `spec/spec [{:keys [form] :as spec}])

(defmethod -responses `s/or [{:keys [form]}]
  (->> form
       (sort-by key)
       (map (fn [[k v]] (response v)))
       (into (array-map))))

(defmethod -responses :default [x])

(defn responses [{:keys [form] :as spec}]
  (some-> form
          (match (comp #{`s/fspec} ::ast/type) #(get % :ret))
          (:ret)
          (-responses)))


;;; Produces

(defn produce [form]
  (-> form (find :headers) (find "Content-Type") :form))

(def -produces nil)
(defmulti -produces ::ast/type)

(defmethod -produces `s/or [{:keys [form]}]
  (->> form
       (map (fn [[_ v]] (produce v)))
       (apply set/union)))

(defn produces [{:keys [form] :as spec}]
  (some-> form
          (match (comp #{`s/fspec} ::ast/type) #(get % :ret))
          (:ret)
          (-produces)))


;;; Parameters

(defn parameter [])
(def -parameters nil)
(defmulti -parameters ::ast/type)
(defmethod -parameters `map? [{:keys [form]}]
  (->> form
       (map (fn [[k v]]
              [(name k)
               (render/render ::json/renderer (:form v))]))
       (into {})))

(defmethod -parameters `s/keys [{:keys [form]}]
  (->> form
       (map (fn [[k v]]
              [(name k)
               (render/render ::json/renderer (:form v))]))
       (into {})))

(defmethod render/render [::parameters `map?] 
  [_ {:keys [form]}]
  (->> form
       ; param type if either query or path
       (mapcat (fn [[param-type {:keys [form]}]]
                 (->> form
                      (map (fn [[k v]]
                             (-> (render/render ::json/renderer v)
                                 (assoc  :name (name k))
                                 (assoc  :in param-type)
                                 (dissoc :schema-name)))))))))

(defn parameters [{:keys [form] :as spec}]
  (clojure.pprint/pprint form)
  (let [query-params (match form
                            (fn [x]
                              (when-let [pname (::ast/name x)]
                                (and (keyword? pname)
                                     (= "query-params" (name pname))))))
        route-params (match form
                            (fn [x]
                              (when-let [pname (::ast/name x)]
                                (and (keyword? pname) (= "route-params" (name pname))))))]
    (concat (map (fn [spec]
                   (-> (render/render ::json/renderer spec)
                       (assoc  :name (name (::ast/name spec)))
                       (assoc  :in "query")
                       (dissoc :schema-name)))
                 (find query-params :req-un))
            (map (fn [spec]
                   (-> (render/render ::json/renderer spec)
                       (assoc  :name (name (::ast/name spec)))
                       (assoc  :in "path")
                       (dissoc :schema-name)))
                 (find route-params :req-un)))))


;;; Rendering
(defn render-method [[k {:keys [handler] :as v}]]
  (let [base       (dissoc v :handler)
        method     (name k)
        ast        (ast/parse handler)
        responses  (some-> ast responses)
        parameters (some-> ast parameters)
        produces   nil;(some-> ast produces)
        ]
    [method (assoc base
                   :responses  responses
                   :parameters parameters
                   :produces   produces)]))

(s/def ::swagger  string?)
(s/def ::basePath string?)
(s/def ::info     map?)

(s/def ::swagger-template
  (s/keys :req-un [::swagger ::base-path]
          :opt-un [::info]))

(defn derive [paths swagger-template]
  ;{:pre [(s/valid? ::swagger-template swagger-template)]}
  (s/assert* ::swagger-template swagger-template)
  (let [defs (atom {})
        extract (partial json/extract-definitions defs)
        paths (->> paths
                   (map (fn [[path {:keys [methods] :as resource}]]
                          [path (->> methods
                                     (map render-method)
                                     (into {}))]))
                   (into {})
                   (walk/postwalk extract))]
    (merge (util/camel-case-keys swagger-template)
           {:paths paths
            :definitions @defs})))
