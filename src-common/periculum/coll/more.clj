(ns periculum.coll.more
  (:require [clj-tuple :as tuples]))

(def empty-vec
  (tuples/tuple))

(defn find-some [p coll]
  (some #(when (p %) %) coll))

(defn mid [coll]
  (let [mid-index (Math/floor ^Float (/ (count coll) 2))]
    (nth coll mid-index)))

(defn min-by [f coll]
  (let [minimum (->> coll (map f) (apply min))
        value (find-some #(= (f %) minimum) coll)]
    (if (map? coll)
      (apply hash-map value)
      value)))

(defn max-by [f coll]
  (let [maximum (->> coll (map f) (apply max))
        value (find-some #(= (f %) maximum) coll)]
    (if (map? coll)
      (apply hash-map value)
      value)))

(defn or-else [f else]
  (fn [x]
    (if-let [_ x]
      (f x)
      else)))

(defn consume [f coll]
  (for [elm coll]
    (let [rem (drop-while #(not (= % elm)) coll)]
      (f elm rem))))

(defn map-assoc [f m]
  (reduce
    (fn [nmap [k v]]
      (assoc nmap k (f k v))) {} m))

(defn pick-rnd [coll]
  (let [th (rand-int (count coll))]
    (if (vector? coll)
      (nth coll th)
      (nth (vec coll) th))))

