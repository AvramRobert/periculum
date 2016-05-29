(ns periculum.examples.rl-triangle-path
  (:use [periculum.rl])
  (:require [clojure.core.match :refer [match]]
            [clojure.core.async :as async]
            [periculum.rl :as rl]))

(defrecord local-state [row index])

(def world
  [     [3]
       [2 4]
      [1 9 3]
     [9 9 2 4]
    [4 6 6 7 8]
   [5 7 3 5 1 4] ])

(defn actions [_]
  [:left :right])

(defn transition [terminal?]
  (fn [S A]
    (if (terminal? S)
      S
      (case A
        :left (->local-state (inc (:row S)) (:index S))
        :right (->local-state (inc (:row S)) (inc (:index S)))
        S))))

(defn rewards [world transition]
  (fn [S A]
    (let [{row   :row
           index :index} (transition S A)]
      (-> world (nth row) (nth index)))))

(def terminal-f #(>= (:row %) (dec (count world))))
(def transition-f (transition terminal-f))
(def reward-f (rewards world transition-f))

(defn find-best [start data]
  (let [f (rl/compute-path greedy-by-min transition-f reward-f terminal-f)]
    (f start data)))

(defn as-reward [chain] (conj (map :reward chain) (first (first world))))

(defn triangle-path [learn-f]
  (->> (learn-f)
       (async/<!!)
       (find-best (->local-state 0 0))
       (as-reward)))

;; works very well with this MDP
(defn run-mc [episodes]
  (triangle-path
    (periculum.dsl/deflearn
      :world world
      :action actions
      :transition transition-f
      :terminal terminal-f
      :reward reward-f
      :algorithm monte-carlo
      :gamma 1.0
      :start (->local-state 0 0)
      :episodes episodes
      :policy (eps-greedy 0.6 greedy-by-min))))

;; SARSA won't work with this MDP, because the reward is dependent on continuation
;; SARSA must be greedy in the limit in order for its convergence to work
;; The problem is, SARSA is also 100% convergent if all states are visited infinitely many times. This I do not do.
(defn run-sarsa-1 [eps]
  (triangle-path
    (periculum.dsl/deflearn
      :world world
      :action actions
      :transition transition-f
      :terminal terminal-f
      :reward reward-f
      :algorithm sarsa-1
      :gamma 1.0
      :alpha 0.2
      :start (->local-state 0 0)
      :episodes eps
      :policy (GLIE-ε-greedy 0.6 greedy-by-min))))

;; SARSA λ also works and depending on alpha, lambda and exploration, it can be more effective than MC
(defn run-sarsa-λ [eps]
  (triangle-path
    (periculum.dsl/deflearn
      :world world
      :action actions
      :transition transition-f
      :terminal terminal-f
      :reward reward-f
      :algorithm sarsa-λ
      :gamma 1.0
      :alpha 0.1
      :lambda 0.9
      :start (->local-state 0 0)
      :episodes eps
      :policy (eps-greedy 0.6 greedy-by-min))))

;; works very well with this MDP
(defn run-q-learning [eps]
  (triangle-path
    (periculum.dsl/deflearn
      :world world
      :action actions
      :transition transition-f
      :terminal terminal-f
      :reward reward-f
      :algorithm (fn [policy action-f reward-f transition-f terminal?]
                   (q-learning greedy-by-min policy action-f reward-f transition-f terminal?))
      :gamma 0.9
      :alpha 0.5
      :start (->local-state 0 0)
      :episodes eps
      :policy (eps-greedy 0.6 greedy-by-min))))