(ns periculum.prepare
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.learning]
    [periculum.more])
  (:require
    [clojure.core.async :as async]
    [play-clj.core :refer [shape, color, color!]]))

(def ^:const local-path "/home/robert/Repositories/periculum/resources/")

(def policy-channel (async/chan (async/buffer 2048)))
(def result-channel (async/chan))

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

(defn observe! [channel]
  (delayed-observer channel))

(def choice-observer (observe! policy-channel))

(defn choice-supplier [algorithm]
  (let [data (conf 1.0 0.4 0.7)
        policy (eps-greedy 0.6 greedy-by-max)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm policy action-f reward-f transition-f terminal-f)]
    (control exp-algorithm data)))


(defn path-supplier [algorithm eps]
  (let [data (conf 1.0 0.2 0.7)
        start (state 1 1 :stand)
        policy (eps-greedy 0.6 greedy-by-max)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm policy action-f reward-f transition-f terminal-f)
        ctrl (control exp-algorithm data)
        best (derive-path greedy-by-max action-f transition-f terminal-f)]
    (let [thread-chan (ctrl start eps)
          new-data (async/<!! thread-chan)
          path (best start new-data)]
      (for [e path]
        (async/>!! result-channel e)))))



