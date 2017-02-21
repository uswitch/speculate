(ns speculate.transform.state
  (:refer-clojure :exclude [concat map mapcat map-indexed merge])
  (:require [speculate.util :as util]))

(alias 'c 'clojure.core)

(defn update-value [value f & args]
  (apply update value 0 f args))

(defn update-state [value f & args]
  (apply update value 1 f args))

(defn reset [value state & ks]
  (update value 1 c/merge (zipmap ks (c/map state ks))))

(defn merge [& state-maps]
  (reduce (fn [[a sa] [b sb]]
            [(c/merge a b) (util/deep-merge sa sb)])
          state-maps))

(defn concat [& state-colls]
  (reduce (fn [[a sa] [b sb]]
            [(c/concat a b) (util/deep-merge sa sb)])
          state-colls))

(defn map [state f & colls]
  (let [heads (c/map first colls)]
    (if (every? some? heads)
      (let [[value state'] (apply f state heads)]
        (let [[values state''] (apply map state' f (c/map rest colls))]
          (if values
            [(c/concat value values) state'']
            [value state'])))
      [nil state])))

(defn mapcat [state f & colls]
  (let [[values state] (apply map state f colls)]
    [(apply c/concat values) state]))

(defn map-indexed [state f & colls]
  (apply map state f (range) colls))
