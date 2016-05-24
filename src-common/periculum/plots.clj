(ns periculum.plots
  (:use
    [incanter.charts]
    [incanter.core]
    [incanter.stats]
    [incanter.datasets])
  (:require [incanter.io :as io]
            [clojure.core.async :as async]
            [periculum.rl :as rl]
            [clj-tuple :as tuples]))

;; Representation is dependent on f
(defn capture [channel f]
  (async/go-loop [gathered (tuples/tuple)]
    (if-let [data (async/<! channel)]
      (let [res (f data)]
        (recur (conj gathered res)))
      (do
        (println "Done")
        gathered))))

(defn merge-data [channel]
  (if-let [data (async/<!! channel)]
    (reduce #(merge-with (fn [l r]
                           (flatten [l r])) %1 %2) data)
    {}))

(defn monitor-greedily [start action-f transition-f reward-f terminal?]
  (let [get-greedy (rl/derive-path action-f transition-f reward-f terminal?)
        transform (fn [data] (do
                               {:episode      (:episode data)
                                :markov-chain (get-greedy start (:data data))
                                :gamma        (-> data :data :gamma)}))]
    (fn [channel f]
      (async/thread
        (-> channel
            (capture #(->> % transform f))
            (merge-data))))))

(defn exp-per-eps [data]
  (let [{episode :episode
         chain   :markov-chain
         gamma   :gamma
         } data
        total (reduce #(+ %1 (:reward %2)) 0.0 (rl/Gt chain gamma))]
    {:episode episode :reward total}))


(defn as-lines [channel]
  (when-let [data (async/<!! channel)]
    (view (line-chart
            :episode :reward
            :data (to-dataset data)))))


