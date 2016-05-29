(ns periculum.play
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.learning]
    [periculum.more]
    [periculum.plots]
    [periculum.dsl])
  (:require [clojure.core.async :as async]))

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

(def world (make-world world-config1))
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

(defn -learn [world & kvs]
  (apply deflearn (-local-prims world kvs)))

(defn -learn-cont [& kvs]
  (apply deflearn-cont (-local-prims world kvs)))
