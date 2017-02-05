(ns speculate.lens
  (:require
   [bifocal.lens :as lens]
   [clojure.spec :as s]
   [speculate.ast :as ast]
   [speculate.spec :as u]
   [speculate.util :as util]))

(defmulti -mk ::ast/type)

(defn mk [{:keys [leaf] :as ast}]
  (if (:leaf ast)
    lens/id
    (-mk ast)))

(defmethod -mk `s/keys [ast]
  (let [descend (fn [lens-f]
                  (fn [ast]
                    (comp (lens-f ast) (mk ast))))
        {:keys [req req-un opt opt-un]} (:form ast)]
    (apply lens/+>>
           (concat
            (map (descend (comp lens/key ::ast/name)) req)
            (map (descend (comp lens/key util/un-ns ::ast/name)) req-un)
            (map (descend (comp lens/key ::ast/name)) opt)
            (map (descend (comp lens/key util/un-ns ::ast/name)) opt-un)))))

(defmethod -mk `s/or [ast]
  (apply lens/cond
         (map (fn [[_ {:keys [::s/name] :as ast}]]
                [(partial s/valid? name) (mk ast)])
              (:form ast))))

(defmethod -mk `s/and [ast]
  (->> (:form ast)
       (filter (fn [{:keys [::s/name ::ast/type]}]
                 (or (s/spec? name) (util/spec-symbol? type))))
       (first)
       (mk)))

(defmethod -mk `s/every [ast]
  (comp lens/map (-> ast :form mk)))

(defmethod -mk `s/nilable [ast]
  (lens/maybe (mk (:form ast))))

(defmethod -mk `s/tuple [ast]
  (apply lens/tuple lens/+>> (map mk (:form ast))))

(defmethod -mk `u/categorize [ast]
  (comp (lens/meta (fn [s]
                     (->> (:categorize ast)
                          (map (fn [[k f]] [k (f s)]))
                          (into {}))))
        (mk (:form ast))))

(defmethod -mk `u/select [ast]
  (comp lens/map
        (mk (:form ast))
        (lens/filter (fn [s]
                       (every? (fn [[k f]] (f (k (meta s)))) (:select ast))))))
