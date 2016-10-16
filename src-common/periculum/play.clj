(ns periculum.play
  (:use
    [periculum.world]
    [periculum.domain]
    [periculum.plots]
    [periculum.dsl])
  (:require [clojure.core.async :as async]
            [periculum.ext :as ops]
            [periculum.genetic :as g]
            [periculum.rl :as rl]
            [periculum.exports :as e]
            [periculum.more :as m]))

(def ^:const local-path "/home/robert/Repositories/periculum/resources/")

(def result-channel (async/chan))
(def expect-channel (async/chan))

(defn state [x y action]
  (->State (->Pos x y) action))

(def world-config1 {:floor     (m-struct 16 0 (pos 0 0))
                    :holes     [(pos 3 0) (pos 4 0) (pos 9 0) (pos 10 0)]
                    :walls     [(m-struct 2 3 (pos 7 1))]
                    :platforms [(m-struct 2 (pos 10 5))]})

(def world-config2 {:floor     (m-struct 14 0 (pos 0 0))
                    :holes     [(pos 3 0) (pos 13 0) (pos 14 0) (pos 15 0)]
                    :walls     [(m-struct 2 3 (pos 6 1)) (m-struct 2 3 (pos 10 1))]
                    :platforms m/empty-vec
                    })

(def world-config3 {:floor     (m-struct 20 0 (pos 0 0))
                    :holes     [(pos 4 0) (pos 5 0) (pos 6 0)
                                (pos 7 0) (pos 8 0) (pos 9 0)
                                (pos 10 0) (pos 11 0) (pos 12 0)]
                    :walls     [(m-struct 2 3 (pos 13 1))]
                    :platforms [(m-struct 2 (pos 5 3)) (m-struct 2 (pos 8 4))]})



(def scene0_stage0 (world-from-pixmap (str local-path "scene0_stage0.png")))
(def scene0_stage1 (world-from-pixmap (str local-path "scene0_stage1.png")))
(def scene0_stage2 (world-from-pixmap (str local-path "scene0_stage2.png")))
(def scene0_start (->State (->Pos 1 1) :stand))

(def scene1_stage0 (world-from-pixmap (str local-path "scene1_stage0.png")))
(def scene1_stage1 (world-from-pixmap (str local-path "scene1_stage1.png")))
(def scene1_stage2 (world-from-pixmap (str local-path "scene1_stage2.png")))
(def scene1_start (->State (->Pos 6 13) :stand))

(def scene2_stage0 (world-from-pixmap (str local-path "scene2_stage0.png")))
(def scene2_stage1 (world-from-pixmap (str local-path "scene2_stage1.png")))
(def scene2_stage2 (world-from-pixmap (str local-path "scene2_stage2.png")))
(def scene2_start0 (->State (->Pos 4 11) :stand))
(def scene2_start1 (->State (->Pos 4 15) :stand))
(def scene2_start2 (->State (->Pos 4 24) :stand))

(def world scene2_stage2)
(def start scene2_start2)

(defn terminals [key]
  (case key
    :scene0_stage0 (terminal? scene0_stage0 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))
    :scene0_stage1 (terminal? scene0_stage1 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 4))))
    :scene0_stage2 (terminal? scene0_stage2 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))

    :scene1_stage0 (terminal? scene1_stage0 (fn [state max] (and
                                                              (>= (-> state :position :x) (- (-> max :position :x) 3))
                                                              (<= (-> state :position :y) 5))))
    :scene1_stage1 (terminal? scene1_stage1 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))
    :scene1_stage2 (terminal? scene1_stage2 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))

    :scene2_stage0 (terminal? scene2_stage0 (fn [state max] (and
                                                              (>= (-> state :position :x) (- (-> max :position :x) 3)))))
    :scene2_stage1 (terminal? scene2_stage1 (fn [state max] (and
                                                              (>= (-> state :position :x) (- (-> max :position :x) 6))
                                                              (<= (-> state :position :y) 7))))
    :scene2_stage2 (terminal? scene2_stage2 (fn [state max] (and
                                                              (>= (-> state :position :x) (- (-> max :position :x) 3))
                                                              (<= (-> state :position :y) 7))))
    "default"))

(defn- add-locals [world config]
  (let [terminal (terminals (terminal<- config))]
    (-> config
        (assoc :action actions)
        (assoc :reward (reward world terminal))
        (assoc :transition (transition world terminal))
        (assoc :terminal terminal))))

(defn recompute [world config]
  (let [locals (add-locals world config)
        start (start<- locals)
        optimum (resolver<- locals)
        mem (atom {})]
    (fn ([] (when (m/not-empty? @mem) (optimum start @mem)))
      ([channel]
       (let [data (async/<!! channel)
             _ (swap! mem (fn [_] data))]
         (optimum start data))))))

(defn re-echo [data]
  (async/>!! result-channel data))

(defn learn [world config]
  "Delegates to `deflearn` but presets the primites to those of the platformer MDP"
  (deflearn (add-locals world config)))

(defn learn-cont [world config]
  "Delegates to `deflearn-cont` but presets the primites to those of the platformer MDP"
  (deflearn-cont (add-locals world config)))

(defn- genetic-traj [config]
  (let [rollout (rollout<- config)
        roll (fn [data S] (rollout data S nil))
        indv# (population<- config)
        gen# (generations<- config)
        select (g/selection (g/n-elitism (elites<- config))
                            (g/roulette (roulette<- config)))]
    (fn [data S]
      (let [genesis (g/genetically
                      (repeat indv# (roll data S))
                      ops/p-eval
                      select
                      (ops/p-mutate data roll)
                      ops/p-cross
                      #(>= (:score %) 2.0))]
        (second (genesis gen#))))))

(defn- q-up [config]
  (let [argmax (rl/greedy rl/greedy-by-max)
        action (action<- config)
        transition (transition<- config)]
    (fn [data sample]
      (let [{S :state
             A :action
             R :reward} sample
            S' (transition S A)
            A' (argmax S' (action S') data nil)]
        (assoc-in data [:q-values S A] (rl/Q-sarsa-1 data S A R S' A'))))))


(defn- dyna [config]
  (let [τ (genetic-traj config)
        update (q-up config)
        evolve (fn [data]
                 (->> (τ data (-> data :q-values keys rand-nth))
                      (g/indv)
                      (reduce #(update %1 %2) data)))]
    (rl/dyna-γ-max evolve)))

(defn magrl [world config]
  "Multi-Agent Genetic Reinforcement Learning"
  (let [locals (add-locals world config)
        follow (resolver<- locals)
        algorithm (algorithm<- locals)
        data (data<- locals)
        start (start<- locals)
        episodes (episodes<- config)
        fitness (ops/vf-eval start follow)
        mutate (ops/vf-mutate 0.5)
        cross (ops/vf-cross-rnd follow)
        repopulate (g/genesis mutate cross)
        fittest #(m/max-by fitness %)
        dispatches (dispatches<- config)
        select (g/selection (g/n-elitism (elites<- config))
                            (g/roulette (roulette<- config)))
        evolve #((g/evolve-w % fitness select repopulate) (generations<- locals))
        run! (rl/control->gen algorithm data evolve fittest dispatches)]
    (fn [] (run! start episodes))))

(defn geprl [world config]
  "Genetically Planned Reinforcement Learing"
  (let [locals (add-locals world config)]
    (->> locals
         (dyna)
         (assoc locals :algorithm)
         (deflearn))))

(defn match-algorithm [algorithm config]
  (case algorithm
    :sarsa-max (fn [world dispatches] (learn world (-> config (assoc :dispatches dispatches))))
    :magrl (fn [world dispatches] (magrl world (assoc config :dispatches dispatches)))
    :geprl (fn [world dispatches] (geprl world (assoc config :dispatches dispatches)))))

(defn experiment-genetics [amount
                           algorithm-key
                           schedule
                           world
                           config]
  "
  amount -> number of experiments
  algorithm-key -> key for the algorithm used
                   current ones are: :sarsa-max, :magrl and :geprl
  schedule -> predicate(episode): states when to echo data to export thread
  world -> world to be used
  config -> standard config provided for any algorithm
            Note: in the case of MAGRL of GEPRL, the genetic
            attributes also need to be added.
            [population, generations, elites, (+ interval)]"
  (let [locals (add-locals world config)
        prep (match-algorithm algorithm-key locals)
        follow #((resolver<- locals) (start<- locals) %)]
    (fn []
      (dotimes [n amount]
        (println "Experiment " n)
        (let [channel (async/chan)
              run! (prep world {:channel  channel
                                :schedule schedule})
              exp (e/exp (name algorithm-key) n)
              decoder (e/path-decode follow #(>= % 1.0))]
          (do
            (e/listen! channel exp decoder)
            (async/<!! (run!))))))))