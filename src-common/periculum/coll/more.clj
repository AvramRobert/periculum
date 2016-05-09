(ns periculum.coll.more
  (:require [clj-tuple :as tuples]))

(def empty-vec
  (tuples/tuple))

(defn find-some [p coll]
  (some #(when (p %) %) coll))

(defn mid [coll]
  (let [mid-index (Math/floor ^Float (/ (count coll) 2))]
    (nth coll mid-index)))

(defn min-by [p coll]
  ((->> coll (map p) (apply min))))

(defn max-by [p coll]
  (->> coll (map p) (apply max)))

(defn or-else [f else]
  (fn [x]
    (if-let [arg x]
      (f x)
      else)))

(defn consume [f coll]
  (loop [whole []
         rem coll]
    (if (empty? rem)
      whole
      (recur (conj whole (f (first rem) rem)) (rest rem)))))

(defn map-assoc [f m]
  (reduce
    (fn [nmap [k v]]
      (assoc nmap k (f k v))) {} m))
