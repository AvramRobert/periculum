(ns periculum.examples.rl-triangle-path
  (:use [periculum.rl])
  (:require [clojure.core.match :refer [match]]))

(defrecord state [row index])

(def actions [:left :right])

(defn transition [terminal?]
  (fn [S A]
    (if (terminal? S)
      S
      (match [A]
             [:left] (->state (inc (:row S)) (:index S))
             [:right] (->state (inc (:row S)) (inc (:index S)))))))

(defn rewards [world transition]
  (fn [S A]
    (let [S' (transition S A)
          {row   :row
           index :index
           } S']
      (-> world (nth row) (nth index)))))

(defn greedy-by-min [As]
  (->> As (opt-by-min) (keys) (first)))

(def world
  [[3] [2 4] [1 9 3] [9 9 2 4] [4 6 6 7 8] [5 7 3 5 1 4]])

(defn path-from-qs [data terminal? transition-f optimal]
  (loop [path [] S (->state 0 0)]
    (if (terminal? S)
      (conj path S)
      (let [As (get (:q-values data) S)
            A (optimal As)]
        (recur (conj path S) (transition-f S A))))))

(defn as-reward [path]
  (map #(-> world (nth (:row %)) (nth (:index %))) path))


;; works very well with this MDP
(defn run-mc [eps]
  (let [data (conf 1.0 0.0 0.0)
        terminal? #(>= (:row %) (dec (count world)))
        transition-f (transition terminal?)
        reward-f (rewards world transition-f)
        policy (ε-greedy 0.6 greedy-by-min)
        action-f (fn [_] actions)
        algorithm (monte-carlo policy action-f reward-f transition-f terminal?)
        env (control algorithm data identity)
        res (env (->state 0 0) eps)
        path (path-from-qs res terminal? transition-f greedy-by-min)
        reward-path (as-reward path)]
    reward-path))

;; SARSA won't work with this MDP, because the reward is dependent on continuation
;; SARSA must be greedy in the limit in order for its convergence to work
;; The problem is, SARSA is also 100% convergent if all states are visited infinitely many times. This I do not do.
(defn run-sarsa [eps]
  (let [data (conf 0.9 1.0 0.0)
        terminal? #(>= (:row %) (dec (count world)))
        transition-f (transition terminal?)
        reward-f (rewards world transition-f)
        policy (GLIE-eps-greedy 0.6 greedy-by-min)
        action-f (fn [_] actions)
        algorithm (sarsa policy action-f transition-f reward-f terminal?)
        env (control algorithm data identity)
        res (env (->state 0 0) eps)
        path (path-from-qs res terminal? transition-f greedy-by-min)
        reward-path (as-reward path)
        ]
    reward-path))

;; SARSA λ also works and depending on alpha, lambda and exploration, it can be more effective than MC
(defn run-sarsa-λ [eps]
  (let [data (conf 0.9 0.1 0.9)
        terminal? #(>= (:row %) (dec (count world)))
        transition-f (transition terminal?)
        reward-f (rewards world transition-f)
        policy (ε-greedy 0.4 greedy-by-min)
        action-f (fn [_] actions)
        algorithm (sarsa-λ policy action-f reward-f transition-f terminal?)
        env (control algorithm data identity)
        res (env (->state 0 0) eps)
        path (path-from-qs res terminal? transition-f greedy-by-min)
        reward-path (as-reward path)]
    reward-path))

;; works very well with this MDP
(defn run-q-learning [eps]
  (let [data (conf 1.0 0.4 0.0)
        terminal? #(>= (:row %) (dec (count world)))
        transition-f (transition terminal?)
        reward-f (rewards world transition-f)
        policy (ε-greedy 0.6 greedy-by-min)
        action-f (fn [_] actions)
        algorithm (q-learning policy greedy-by-min action-f transition-f reward-f terminal?)
        env (control algorithm data identity)
        res (env (->state 0 0) eps)
        path (path-from-qs res terminal? transition-f greedy-by-min)
        reward-path (as-reward path)]
    reward-path))