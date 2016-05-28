(ns periculum.more
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

(defn map-vals [f map]
  (reduce
    (fn [nmap [k v]]
      (assoc nmap k (f k v))) {} map))

(defn map-keys [f map]
  (reduce
    (fn [nmap [k v]]
      (assoc nmap (f k) v)) {} map))

(defn map-both [kf vf map]
  (reduce
    (fn [nmap [k v]]
      (assoc nmap (kf k) (vf v))) {} map))

(defn filter-kv [pred map]
  (reduce
    (fn [nmap [k v]]
      (if (pred k v)
        (assoc nmap k v)
        nmap)) {} map))

(defn take-while+ [pred coll]
  (loop [acc (tuples/tuple)
         [h & tail] coll]
    (if (pred h)
      (recur (conj acc h) tail)
      (conj acc h))))

(defn apply-n [n f init]
  (loop [value init
         cur n]
    (if (<= cur 0)
      value
      (recur (f value) (dec cur)))))

(defn apply-while [pred f init]
  (loop [value init]
    (if (pred value)
      (recur (f value))
      (f value))))

(defn last-or [coll else]
  (if (empty? coll)
    else
    (last coll)))

(defn foreach [f coll]
  (reduce #(let [_ (f %2)]
            %1) nil coll))

(defn pad-left-to [n coll]
  (let [size (count coll)]
    (if (= size n)
      coll
      (-> coll
          (list)
          (into (take (- n size) (repeat 0)))
          flatten))))

(defn mse [predicted expected]
  (->>
    (zipmap predicted expected)
    (map (fn [[p e]] (Math/pow (- e p) 2)))
    (reduce +)
    (* (/ 1 (count predicted)))))

(defn rmse [predicted expected]
  (Math/sqrt (mse predicted expected)))