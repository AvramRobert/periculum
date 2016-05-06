(ns periculum.coll.more
  (:require [clj-tuple :as tuples]))

(def empty-vec
  (tuples/tuple))

(defn find-some [p coll]
  (some #(when (p %) %) coll))

(defn min-by [p coll]
  ((->> coll (map p) (apply min))))

(defn max-by [p coll]
  (->> coll (map p) (apply max)))