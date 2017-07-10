(ns speculate.json-schema
  (:refer-clojure :exclude [type])
  (:require
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [speculate.util :as util]
   [speculate.ast :as ast]
   [speculate.render :refer [render]]))

(def description-keys
  #{:clojure.spec.alpha/name :name :type :title :description})

(def validation-keys
  {;; number
   :minimum               'number
   :exclusive-minimum     'number
   :maximum               'number
   :exclusive-maximum     'number
   :multiple-of           'number
   ;; string
   :max-length            'string
   :min-length            'string
   :pattern               'string
   ;; array
   :min-items             'array
   :max-items             'array
   :items                 'array
   :additional-items      'array
   :unique-items          'array
   ;; objects
   :properties            'object
   :max-properties        'object
   :min-properties        'object
   :pattern-properties    'object
   :additional-properties 'object
   :required              'object
   :dependencies          'object
   ;; enum
   :enum                  'enum})

(def schema-keys
  (set/union description-keys (set (keys validation-keys))))

(defmulti type identity)

;; Simple Types
(defmethod type nil          [_] {:type 'null})
(defmethod type `nil?        [_] {:type 'null})
(defmethod type `boolean?    [_] {:type 'boolean})
(defmethod type `string?     [_] {:type 'string})

;; Named
(defmethod type `keyword?    [_] {:type 'string})
(defmethod type `symbol?     [_] {:type 'string})

;; Numbers
(defmethod type `char?       [_] {:type 'string})
(defmethod type `int?        [_] {:type 'integer :format 'int32})
(defmethod type `integer?    [_] {:type 'integer :format 'int32})
(defmethod type `pos-int?    [_] {:type 'integer :format 'int32 :minimum 1})
(defmethod type `nat-int?    [_] {:type 'integer :format 'int32 :minimum 0})
(defmethod type `bigdec?     [_] {:type 'long    :format 'int64})
(defmethod type `decimal?    [_] {:type 'double})
(defmethod type `double?     [_] {:type 'double})
(defmethod type `float?      [_] {:type 'float})
(defmethod type `number?     [_] {:type 'float})
(defmethod type `ratio?      [_] {:type 'float})
(defmethod type `rational?   [_] {:type 'float})

;; Domain Types
(defmethod type `uri?        [_] {:type 'string :format 'url})
(defmethod type `uuid?       [_] {:type 'string :format 'uuid})

;; Sequences
(defmethod type `coll?       [_] {:type 'array})
(defmethod type `list?       [_] {:type 'array})
(defmethod type `seq?        [_] {:type 'array})
(defmethod type `sequential? [_] {:type 'array})
(defmethod type `vector?     [_] {:type 'array})

;; Sets
(defmethod type `set?        [_] {:type 'array})

;; Maps
(defmethod type `map?        [_] {:type 'object})

;; Clojure Classes
(defmethod type clojure.lang.Keyword    [_] {:type 'string})
(defmethod type clojure.lang.Symbol     [_] {:type 'string})

;; Java Classes
(defmethod type java.lang.Integer       [_] {:type 'integer :format 'int32})
(defmethod type java.lang.Long          [_] {:type 'long    :format 'int64})
(defmethod type java.lang.Double        [_] {:type 'number :format 'double})
(defmethod type java.lang.Number        [_] {:type 'number :format 'double})
(defmethod type java.lang.String        [_] {:type 'string})
(defmethod type java.lang.Boolean       [_] {:type 'boolean})
(defmethod type java.util.UUID          [_] {:type 'string :format 'uuid})
(defmethod type java.util.Date          [_] {:type 'string :format 'date-time})
(defmethod type java.util.regex.Pattern [_] {:type 'string :format 'regex})

(defmethod type :default [_])

(defn derive-set-type [s]
  (let [[t :as ts] (map class s)]
    (if (every? (partial = t) ts)
      (type t)
      (distinct (remove nil? (map type ts))))))

(defmethod render [::renderer 'clojure.core/set?]
  [_ {:keys [form] :as x}]
  (merge {:enum form
          :type (derive-set-type form)}
         (select-keys x schema-keys)))

(defmethod render [::renderer 'clojure.core/map?]
  [_ {:keys [form] :as x}]
  (->> form
       (map (fn [[k v]] [k (render ::renderer v)]))
       (into {})))

(defmethod render [::renderer 'clojure.core/symbol?]
  [_ {:keys [form] :as x}]
  (merge (type form) (select-keys x schema-keys)))

(defmethod render [::renderer 'clojure.spec.alpha/keys]
  [_ {{:keys [req req-un opt opt-un]} :form :as spec}]
  (let [pname (::ast/name spec)
        title (some-> pname name util/pascal-case)
        properties (->> (concat req req-un opt opt-un)
                        (map (juxt (comp name ::ast/name)
                                  (partial render ::renderer)))
                        (into {}))
        base (cond-> {:type 'object
                      :properties properties
                      :required (mapv (comp name ::ast/name)
                                      (concat req req-un))}
               pname (assoc :schema-name pname)
               title (assoc :title title))]
    (merge base (select-keys spec schema-keys))))

(defmethod render [::renderer 'clojure.spec.alpha/every]
  [_ {:keys [form] :as spec}]
  ;; this could also be a map-of
  ;; (and (form? form) (= `s/tuple (first form)))
  ;; (merge-spec-meta {:type 'object :properties })
  (let [base {:type  'array
              :items (render ::renderer form)}]
    (merge base (select-keys spec schema-keys))))

(defmethod render [::renderer 'clojure.spec.alpha/coll-of]
  [_ {:keys [form] :as spec}]
  ;; this could also be a map-of
  ;; (and (form? form) (= `s/tuple (first form)))
  ;; (merge-spec-meta {:type 'object :properties })
  (let [base {:type  'array
              :items (render ::renderer form)}]
    (merge base (select-keys spec schema-keys))))

(defmethod render [::renderer 'clojure.spec.alpha/and]
  [_ {:keys [form] :as spec}]
  (merge (reduce merge {} (map (partial render ::renderer) form)) spec))

(defmethod render [::renderer 'clojure.spec.alpha/or]
  [_ {:keys [form]}]
  {:one-of (map (comp (partial render ::renderer) second) form)})

(defmethod render [::renderer 'speculate.spec/spec]
  [_ {:keys [form] :as m}]
  (let [pname (::ast/name m)
        title (some-> pname name util/pascal-case)
        base (cond-> (render ::renderer form)
               :trim (select-keys schema-keys)
               pname (assoc :schema-name pname)
               title (assoc :title title))]
    (merge base (select-keys m schema-keys))))

(defmethod render [::renderer ::URI]
  [_ {:keys [::base-uri] :as ast}]
  (let [pname (::ast/name ast)]
    {"$ref" (format "%s/%s/%s"
                    base-uri
                    (some-> pname namespace (string/replace #"\." "/"))
                    (name pname))}))

(defmethod render [::renderer ::root]
  [_ {:keys [::base-uri form] :as ast}]
  (let [pname (::ast/name form)
        sub   (render ::renderer form)]
    (merge sub
           {"$schema" (format "%s/%s/%s"
                              base-uri
                              (some-> pname namespace (string/replace #"\." "/"))
                              (name pname))})))

(defn extract-definitions [atom x]
  (if (and (map? x) (contains? x :schema-name))
    (let [{title :title} x
          x (dissoc x :schema-name)]
      (swap! atom assoc title x)
      {"$ref" (format "#/definitions/%s" title)})
    x))

(defn schema [ast & {:keys [extract-definitions?]}]
  (let [rendered (render ::renderer ast)]
    (if extract-definitions?
      (let [defs (atom {})
            extract (partial extract-definitions defs)
            base (->> (dissoc rendered :schema-name)
                      (map (fn [[k v]] [k (walk/postwalk extract v)]))
                      (into {}))]
        (if (seq @defs)
          (assoc {(:title base) base} :definitions @defs)
          base))
      (walk/postwalk #(cond-> % (map? %) (dissoc :schema-name)) rendered))))
