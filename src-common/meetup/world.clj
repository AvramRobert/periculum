(ns meetup.world
  (:require [periculum.domain :as d]
            [periculum.play :refer [world start result-channel]]
            [periculum.dsl :refer [deflearn]]
            [clojure.core.async :as async]
            [periculum.rl :as rl]))

(defn eps-greedy [eps]
  (fn [S As data]
    (if-let [Q-sa (get data S)]
      (let [p-random (/ eps (count As))
            p-greedy (+ (- 1.0 eps) p-random)
            greedy-A (or (some->> Q-sa (periculum.more/max-by val) (keys) (first))
                         (rand-nth As))]
        (->> As
             (map #(if (= greedy-A %) p-greedy p-random))
             (periculum.more/choose-dist As)))
      (rand-nth As))))

(def start-state start)

(defn terminal? [state]
  ((d/terminal? world) state))

(defn transition [state action]
  ((d/transition world terminal?) state action))

(defn reward [state action]
  ((d/reward world terminal?) state action))

(defn actions [S] (d/actions S))