(ns periculum.plots
  (:use
    [incanter.charts]
    [incanter.core]
    [incanter.stats]
    [incanter.datasets]
    [periculum.more])
  (:require [clojure.core.async :as async]
            [periculum.rl :as rl]
            [clj-tuple :as tuples]))

(defn action-index [action]
  (case action
    :stand 0
    :walk-left 1
    :walk-right 2
    :run-left 3
    :run-right 4
    :jump-up 5
    :jump-left 6
    :jump-right 7
    :run-jump-left 8
    :run-jump-right 9))

(defn- apply-options [plot options]
  (loop [o-plot plot
         opts options]
    (if (empty? opts)
      o-plot
      (let [k (first opts)
            v (second opts)
            rem (drop 2 opts)]
        (case k
          :title (recur (set-title plot v) rem)
          :x-label (recur (set-x-label plot v) rem)
          :y-label (recur (set-y-label plot v) rem)
          (recur o-plot rem))))))

(defn- plot-lines! [x-data y-data & options]
  (doto
    (apply-options (xy-plot) options)
    (add-lines x-data y-data)
    view))

(defn capture [channel f]
  (loop [gathered (tuples/tuple)]
    (if-let [data (async/<!! channel)]
      (do
        (recur (conj gathered data)))
      (do
        (println "Done")
        (map f gathered)))))

(defn monitor [channel f & gs]
  (async/thread
    (let [all (if (empty? gs)
                identity
                (apply comp (reverse gs)))]
      (-> channel
          (capture f)
          all))))

(defn capture-greedy [start transition-f reward-f terminal?]
  (let [get-greedy (rl/derive-path transition-f reward-f terminal?)]
    (fn [data]
      {:episode      (:episode data)
       :markov-chain (get-greedy start (:data data))
       :gamma        (-> data :data :gamma)})))

(defn- merged-lines!
  ([title
    x-label y-label data]
   (let [st (reduce #(merge-with (fn [l r]
                                   (flatten [l r])) %1 %2) data)]
     (plot-lines! (:x st) (:y st)
                  :title title
                  :x-label x-label
                  :y-label y-label))))

(defn- multiple-charts!
  [title x-label y-label data]
  (foreach
    (fn [{episode :episode
          x-data  :x
          y-data  :y}]
      (view
        (line-chart
          x-data y-data
          :x-label x-label
          :y-label y-label
          :title (str title ":: Episode " episode)))) data))

(defn- scatter-in!
  [title x-label y-label data]
  (letfn [(set-opt [plot]
            (apply-options plot
                           [:title title
                            :x-label x-label
                            :y-label y-label]))]
    (->> data
         (reduce (fn [plot {x-data :x
                            y-data :y}]
                   (add-points plot x-data y-data)) (scatter-plot))
         (set-opt)
         view)))

(defn- scatter-out!
  [title x-label y-label data]
  (letfn [(set-opt [plot episode]
            (apply-options plot
                           [:title (str title " :: Episode " episode)
                            :x-label x-label
                            :y-label y-label]))]
    (foreach
      (fn [{episode :episode
            x-data  :x
            y-data  :y}]
        (-> (scatter-plot)
            (add-points x-data y-data)
            (set-opt episode)
            view)) data)))

(defn- multiple-bars!
  [title x-label y-label data]
  (foreach
    (fn [{episode :episode
          x-data  :x
          y-data  :y}]
      (view (bar-chart
              x-data y-data
              :x-label x-label
              :y-label y-label
              :title (str title " :: Episode " episode)))) data))

(defn- rew|eps [data]
  (map (fn [{episode :episode
             chain   :markov-chain}]
         (let [total (reduce #(+ %1 (:reward %2)) 0.0 chain)]
           {:x episode :y total})) data))

(defn- rew|act [data]
  (map
    (fn [{episode :episode
          chain   :markov-chain}]
      {:episode episode
       :x       (map :action chain)
       :y       (map :reward chain)}) data))

(defn- act|eps [data]
  (let [z (map
            (fn [{episode :episode
                  chain   :markov-chain}]
              {:x episode
               :y (count chain)}) data)]
    z))

(defn- mse|eps [expectation-chain data]
  (map
    (fn [{episode :episode
          chain   :markov-chain}]
      (let [largest (->> [expectation-chain chain] (max-by count) count)
            predicted (->> chain (map :reward) (pad-left-to largest))
            expected (->> expectation-chain (map :reward) (pad-left-to largest))]
        {:x episode
         :y (rmse predicted expected)}))
    data))

(defn reward-per-episode
  ([]
   (reward-per-episode ""))
  ([title]
   (fn [data]
     (->> data
          (rew|eps)
          (merged-lines!
            title
            "Episodes"
            "Rewards")))))

(defn reward-per-action
  ([]
   (reward-per-action ""))
  ([title]
   (fn [data]
     (->> data
          (rew|act)
          (multiple-charts!
            title
            "Actions"
            "Rewards")))))

(defn steps-per-episode
  ([]
   (steps-per-episode ""))
  ([title]
   (fn [data]
     (->> data
          (act|eps)
          (merged-lines!
            title
            "Episodes"
            "Steps")))))

(defn scattered-rewards-per-action
  ([]
   (scattered-rewards-per-action ""))
  ([title]
   (fn [data]
     (->> data
          (rew|act)
          (map (fn [d]
                 (update d :x #(map action-index %))))
          (scatter-out! title
                        "Actions"
                        "Rewards")))))

(defn mse-per-epsiode
  ([expectation-chain]
   (mse-per-epsiode expectation-chain ""))
  ([expectation-chain title]
   (fn [data]
     (->> data
          (mse|eps expectation-chain)
          (merged-lines! title
                         "Episodes"
                         "Mean squared error")))))
