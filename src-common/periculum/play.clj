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
            [periculum.domain :as d]))

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

(defn- -local-prims [w kvs]
  (let [t? (terminal? w)
        add [actions :action
             (terminal? w) :terminal
             (reward w t?) :reward
             (transition w t?) :transition
             t? :terminal]
        args ((or-else
                (fn [_]
                  add) (conj add (state 1 1 :stand) :start)) (val-of kvs :start))]
    (into kvs args)))


(defn recompute [w & kvs]
  (let [vs (-local-prims w kvs)
        start (val-of vs :start)
        optimum (rl/compute-path
                  (val-of vs :transition)
                  (val-of vs :reward)
                  (val-of vs :terminal))
        mem (atom {})]
    (fn
      ([]
       (when (not (empty? @mem))
         (optimum start @mem)))
      ([channel]
       (let [data (async/<!! channel)
             _ (swap! mem (fn [_] data))]
         (optimum start data))))))

(defn re-echo [data]
  (async/>!! result-channel data))

(defn learn [world & kvs]
  "Delegates to `deflearn` but presets the primites to those of the platformer MDP"
  (apply deflearn (-local-prims world kvs)))

(defn learn-cont [& kvs]
  "Delegates to `deflearn-cont` but presets the primites to those of the platformer MDP"
  (apply deflearn-cont (-local-prims world kvs)))

(defn xgen [algorithm mutate cross]
  (let [repopulate (g/genesis mutate cross)]
    (fn [amount population]
      (->> population
           (map #(async/<!! (algorithm %)))
           (repopulate amount)))))

(defn learn-vf [world & kvs]
  (let [vs (-local-prims world kvs)
        follow (rl/compute-path
                 (val-of vs :transition)
                 (val-of vs :reward)
                 (val-of vs :terminal))
        data (rl/conf (val-of vs :gamma)
                      (val-of vs :alpha)
                      (val-of vs :lambda))
        start (val-of vs :start)
        algorithm (fn [ndata] ((apply deflearn (conj vs ndata :data))))]
    (fn [indv# gen# elite#]
      (let [population (map (fn [_] data) (range 0 indv#))
            mutate (ops/vf-mutate 0.5)
            cross (ops/vf-cross-rnd follow)
            fitness (ops/vf-eval start follow)
            perfect? #(= (:score %) 2.0)
            genesis (g/evolve population fitness (xgen algorithm mutate cross) perfect?)]
        (genesis gen# elite#)))))

(defn gen-traj [policy
                action
                reward
                transition
                terminal?]
  (let [rollout (rl/rollout policy action reward transition terminal?)
        roll (fn [data S] (rollout data S nil))]
    (fn [data S indv# gen# elite#]
      (let [population (map (fn [_] (roll data S)) (range 0 indv#))
            mutate (ops/p-mutate data roll)
            genesis (g/genetically population ops/p-eval mutate ops/p-cross #(>= (:score %) 2.0))]
        (second (genesis gen# elite#))))))

(defn q-up [argmax
            action
            transition]
  (fn [data sample]
    (let [{S :state
           A :action
           R :reward} sample
          S' (transition S A)
          A' (argmax S' (action S') data nil)]
      (assoc-in data [:q-values S A] (rl/Q-sarsa-1 data S A R S' A')))))

(defn learn-plan [world & kvs]
  (let [vs (-local-prims world kvs)
        τ (gen-traj rl/eps-balanced
                    (val-of vs :action)
                    (val-of vs :reward)
                    (val-of vs :transition)
                    (val-of vs :terminal))
        update (q-up (rl/greedy rl/greedy-by-max)
                     (val-of vs :action)
                     (val-of vs :transition))]
    (fn [indv# gen# elite#]
      (letfn [(evolve [data]
                (->> elite#
                     (τ data (-> data :q-values keys rand-nth) indv# gen#)
                     (:individual)
                     (reduce #(update %1 %2) data)))]
        ((apply deflearn (conj vs (rl/dyna-γ-max evolve) :algorithm)))))))

(defn traj-opt [indv# gen# elite#]
  (let [term (d/terminal? world)
        trans (d/transition world term)
        rew (d/reward world term)
        f (gen-traj rl/eps-balanced d/actions rew trans term)]
    (f (rl/conf 1.0) start indv# gen# elite#)))