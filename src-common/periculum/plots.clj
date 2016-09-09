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
  (let [get-greedy (rl/compute-path transition-f reward-f terminal?)]
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

(defn- rew|eps [data]
  (map (fn [{episode :episode
             chain   :markov-chain}]
         (let [total (rl/total-reward chain)]
           {:x episode :y total})) data))

(defn- rew|act [data]
  (map
    (fn [{episode :episode
          chain   :markov-chain}]
      {:episode episode
       :x       (map-indexed
                  (fn [idx p]
                    (-> p :action (name) (str "-" idx) (keyword))) chain)
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

;; BIASED TOWARDS MY MDP
;; It should actually directly use the state representation, instead of the transformation
(defn- value|state [data]
  (map
    (fn [{episode       :episode
          learning-data :data}]
      (letfn [(divide [values]
                {:episode episode
                 :x       (keys values)
                 :y       (vals values)})]
        (->>
          learning-data
          :q-values
          (map-kv (fn [k]
                    (str (-> k :position :x)
                         ":"
                         (-> k :position :y)
                         "-"
                         (:previous-action k)))
                  rl/action-mean)
          divide))) data))

(defn reward-per-episode
  "Plots the total reward of each optimum of every episode"
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
  "Plots the reward of each action of an optimum"
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
  "Plots the total amount of action-steps of each optimum
  at every episode"
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

(defn mse-per-epsiode
  "Given an expectation, it plots the mean-squared error relative to each optimum
  of every episode"
  ([expectation-chain]
   (mse-per-epsiode expectation-chain ""))
  ([expectation-chain title]
   (fn [data]
     (->> data
          (mse|eps expectation-chain)
          (merged-lines! title
                         "Episodes"
                         "MSE")))))

(defn value-per-state
  "Plots the complete q-values.
  This represents the domain and codomain of the learned policy"
  ([]
   (value-per-state ""))
  ([title]
   (fn [data]
     (->> data
          (value|state)
          (multiple-charts! title
                            "States"
                            "Values")))))
