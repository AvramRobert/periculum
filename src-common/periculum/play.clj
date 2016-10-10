(ns periculum.play
  (:use
    [periculum.world]
    [periculum.domain]
    [periculum.more]
    [periculum.plots]
    [periculum.dsl])
  (:require [clojure.core.async :as async]
            [periculum.ext :as ops]
            [periculum.genetic :as g]
            [periculum.rl :as rl]
            [periculum.domain :as d]
            [periculum.exports :as e]))

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
                    :platforms empty-vec
                    })

(def world-config3 {:floor     (m-struct 20 0 (pos 0 0))
                    :holes     [(pos 4 0) (pos 5 0) (pos 6 0)
                                (pos 7 0) (pos 8 0) (pos 9 0)
                                (pos 10 0) (pos 11 0) (pos 12 0)]
                    :walls     [(m-struct 2 3 (pos 13 1))]
                    :platforms [(m-struct 2 (pos 5 3)) (m-struct 2 (pos 8 4))]})

(def world (make-world world-config3))
(def start (->State (->Pos 1 1) :stand))
;(def world (world-from-pixmap (str local-path "level1.png")))

(defn- add-locals [world config]
  (-> config
      (assoc :action actions)
      (assoc :reward (reward world (terminal? world)))
      (assoc :transition (transition world (terminal? world)))
      (assoc :terminal (terminal? world))
      (update :start #(if-some [x %] x (state 1 1 :stand)))))


(defn recompute [world config]
  (let [locals (add-locals world config)
        start (start<- locals)
        optimum (resolver<- locals)
        mem (atom {})]
    (fn ([] (when (not-empty? @mem) (optimum start @mem)))
      ([channel]
       (let [data (async/<!! channel)
             _ (swap! mem (fn [_] data))]
         (optimum start data))))))

(defn re-echo [data]
  (async/>!! result-channel data))

(defn learn [config]
  "Delegates to `deflearn` but presets the primites to those of the platformer MDP"
  (deflearn (add-locals world config)))

(defn learn-cont [config]
  "Delegates to `deflearn-cont` but presets the primites to those of the platformer MDP"
  (deflearn-cont (add-locals world config)))

(defn- genetic-traj [config]
  (let [rollout (rollout<- config)
        roll (fn [data S] (rollout data S nil))
        indv# (population<- config)
        gen# (generations<- config)
        elite# (generations<- config)]
    (fn [data S]
      (let [genesis (g/genetically
                      (repeat indv# (roll data S))
                      ops/p-eval
                      (ops/p-mutate data roll)
                      ops/p-cross
                      #(>= (:score %) 2.0))]
        (second (genesis gen# elite#))))))

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
        fittest #(max-by fitness %)
        dispatches (dispatches<- config)
        evolve #((g/evolve-w % fitness repopulate) (generations<- locals) (elites<- config))
        run! (rl/control->gen algorithm data evolve fittest dispatches)]
    (fn [] (run! start episodes))))

(defn geprl [world config]
  "Genetically Planned Reinforcement Learing"
  (let [locals (add-locals world config)]
    (->> locals
         (dyna)
         (assoc locals :algorithm)
         (deflearn))))


(defn magrl-test [world config]
  (let [locals (add-locals world config)
        follow #((resolver<- locals) start %)
        channel (async/chan 256)
        magrl! (magrl world (assoc locals :dispatches {:channel  channel
                                                       :schedule #(= (mod % 2) 0)}))
        exp (e/exp "magrl" 2)
        decoder (e/path-decode follow #(>= % 1.0))]
    (fn []
      (do
        (e/listen! channel exp decoder)
        (magrl!)))))

(defn geprl-test [world config]
  (let [locals (dissoc (add-locals world config) :algorithm)
        follow #((resolver<- locals) start %)
        channel (async/chan 256)
        geprl! (geprl world (assoc locals :dispatches {:channel  channel
                                                       :schedule #(= (mod % 2) 0)}))
        exp (e/exp "geprl" 1)
        decoder (e/path-decode follow #(>= % 1.0))]
    (fn []
      (do
        (e/listen! channel exp decoder)
        (geprl!)))))