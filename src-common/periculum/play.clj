(ns periculum.play
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.domain]
    [periculum.more]
    [periculum.plots]
    [periculum.dsl])
  (:require [clojure.core.async :as async]
            [periculum.ext :as ops]
            [periculum.genetic :as g]))

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
        optimum (compute-path
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

(defn -learn-gen [world & kvs]
  (fn [data]
    ((apply deflearn (-local-prims world (conj kvs data :data))))))

(defn -learn [world & kvs]
  "Delegates to `deflearn` but presets the primites to those of the platformer MDP"
  (apply deflearn (-local-prims world kvs)))

(defn -learn-cont [& kvs]
  "Delegates to `deflearn-cont` but presets the primites to those of the platformer MDP"
  (apply deflearn-cont (-local-prims world kvs)))

(defn run-vf [algorithm data]
  (fn [indv# gen# elite#]
    (let [population (map (fn [_] data) (range 0 indv#))
          fitness (ops/vf-eval start world)
          mutate (ops/vf-mutate-a 0.5)
          perfect? #(= (:score %) 2)
          genesis (g/xevolve population fitness mutate ops/vf-cross-1 perfect?)]
      (genesis gen# elite# #(async/<!! (algorithm %))))))