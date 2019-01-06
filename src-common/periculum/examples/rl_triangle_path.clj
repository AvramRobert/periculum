(ns periculum.examples.rl_triangle_path
  (:use [periculum.rl])
  (:require [clojure.core.match :refer [match]]
            [clojure.core.async :as async]
            [periculum.rl :as rl]
            [periculum.dsl :as dsl]))

;; An example usage of the Reinforcement Learning API.

;; The problem solved here is that of the triangle path.
;; Given a triangle of weighted cells similar to the one below (see `world`),
;; find the most minimal path from top to bottom, whereby the value of
;; each path is the sum of all the visited cells.
;; The single rule is that you are allowed to go only left down or right down
;; from you current position.

;; For this `world`, the optimum is: (3 4 3 2 7 1)

(defrecord cell [row index])

(def world
  [     [3]
       [2 4]
      [1 9 3]
     [9 9 2 4]
    [4 6 6 7 8]
   [5 7 3 5 1 4]])

(def nil-cell (->cell 0 0))

(defn transitions [terminal?]
  (fn [S A]
    (cond (terminal? S) S
          (= A :left) (->cell (inc (:row S)) (:index S))
          (= A :right) (->cell (inc (:row S)) (inc (:index S)))
          :else S)))

(defn rewards [world transition]
  (fn [S A]
    (let [{row   :row
           index :index} (transition S A)]
      (-> world (nth row) (nth index)))))

(def actf (fn [_] [:left :right]))
(def termf #(>= (:row %) (dec (count world))))
(def transf (transitions termf))
(def rewf (rewards world transf))

(defn find-best [start data]
  (let [f (rl/compute-path greedy-by-min transf rewf termf)]
    (f start data)))

(defn as-reward [chain] (into (map :reward chain) (first world)))

(defn triangle-path [learn-f]
  (->> (learn-f)
       (async/<!!)
       (find-best nil-cell)
       (as-reward)))

;; Monte Carlo works very well with this MDP
(defn run-mc [episodes]
  (triangle-path
    (dsl/deflearn
      {:world world
       :action actf
       :transition transf
       :terminal termf
       :reward rewf
       :alpha 0.1
       :algorithm monte-carlo
       :gamma 1.0
       :start (->cell 0 0)
       :episodes episodes
       :policy (eps-greedy 0.6 greedy-by-min)})))

;; SARSA-1 works pretty well with this MDP
(defn run-sarsa-1 [eps]
  (triangle-path
    (dsl/deflearn
      {:world world
       :action actf
       :transition transf
       :terminal termf
       :reward rewf
       :algorithm sarsa-1
       :gamma 1.0
       :alpha 0.2
       :start (->cell 0 0)
       :episodes eps
       :policy (GLIE-eps-greedy 0.6 greedy-by-min)})))

;; SARSA λ works pretty well with this MDP
(defn run-sarsa-λ [eps]
  (triangle-path
    (dsl/deflearn
      {:world world
       :action actf
       :transition transf
       :terminal termf
       :reward rewf
       :algorithm sarsa-λ
       :gamma 1.0
       :alpha 0.3
       :lambda 0.4
       :start (->cell 0 0)
       :episodes eps
       :policy (eps-greedy 0.6 greedy-by-min)})))

;; Q-Learning works very well with this MDP
(defn run-q-learning [eps]
  (triangle-path
    (dsl/deflearn
      {:world world
       :action actf
       :transition transf
       :terminal termf
       :reward rewf
       :algorithm (fn [policy action-f reward-f transition-f terminal?]
                    (q-learning policy (greedy greedy-by-min) action-f reward-f transition-f terminal?))
       :gamma 0.9
       :alpha 0.5
       :start (->cell 0 0)
       :episodes eps
       :policy (eps-greedy 0.6 greedy-by-min)})))