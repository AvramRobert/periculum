(ns periculum.rl-triangle-path
  (:use [periculum.rl])
  (:require [clojure.core.match :refer [match]]))

(defrecord state [row index])

(def actions [:left :right])

(defn rewards [config trans! is-end?]
  (fn [S A]
    (let [S' (trans! S A)
          row (:row S')
          index (:index S')]
      (if (is-end? S)
        0
        (-> config (nth row) (nth index))))))

(defn transition-f [S A]
  (match [A]
         [:left] (state. (inc (:row S)) (:index S))
         [:right] (state. (inc (:row S)) (inc (:index S)))
         :else (println (str "State: " S " :: Action: " A))))

(defn optimum [As]
  (->> As (opt-by-min) (keys) (first)))

(def world
  [     [3]
       [2 4]
      [1 9 3]
     [9 9 2 4]
    [4 6 6 7 8]
   [5 7 3 5 1 4]])

(defn run [eps]
  (let [data (conf 1.0 0.0 0.0)
        is-end? #(>= (:row %) (dec (count world)))
        reward-f (rewards world transition-f is-end?)
        policy (ε-greedy 0.6 optimum)
        action-f (fn [_] actions)
        algorithm (monte-carlo policy action-f reward-f transition-f is-end?)
        env (control algorithm data identity)
        res (env (->state 0 0) eps)]
    res))