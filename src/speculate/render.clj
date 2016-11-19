(ns speculate.render)

(defmulti render (fn [renderer x] [renderer (:speculate.ast/type x)]))

(defmethod render [::abbrev 'clojure.core/set?]
  [_ {:keys [form]}]
  (str (first form)))

(defmethod render :default
  [renderer x]
  (cond (string? x) x
        :default x))
