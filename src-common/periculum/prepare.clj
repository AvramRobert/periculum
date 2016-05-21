(ns periculum.prepare
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.learning]
    [periculum.more])
  (:require
    [clojure.core.async :as async]
    [play-clj.core :refer [shape, color, color!]]))

(def block-size 64)
(def half-block (/ block-size 2))

(def policy-channel (async/chan (async/buffer 2048)))
(def command-chan (async/chan))

(defn state [x y action]
  (->State (->Pos x y) action))

;(def world-config {:floor     (m-struct 16 0 (pos 0 0))
;                   :holes     [(pos 3 0) (pos 4 0) (pos 9 0) (pos 10 0)]
;                   :walls     [(m-struct 2 3 (pos 7 1))]
;                   :platforms [(m-struct 2 (pos 10 5))]})

(def world-config {:floor     (m-struct 14 0 (pos 0 0))
                   :holes     [(pos 3 0) (pos 13 0) (pos 14 0) (pos 15 0)]
                   :walls     [(m-struct 2 3 (pos 6 1)) (m-struct 2 3 (pos 10 1))]
                   :platforms empty-vec
                   })

(def world (make-world world-config))

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
        (async/>!! command-chan e)))))

(defn block-pos
  ([x y]
   (pos (* x block-size) (* y block-size)))
  ([{x :x y :y}]
   (block-pos x y)))

(defn derive-pos
  [entity]
  (let [x (get-in entity [:position :x])
        y (get-in entity [:position :y])]
    (block-pos x y)))

(defn observe! [channel]
  (delayed-observer channel))

(defn do-trans! [entity pair]
  (let [pos (block-pos (->> pair (:state) (:position)))
        body (:body entity)]
    (case body
      :geometric (-> entity (assoc :x (+ (:x pos) half-block)
                                   :y (+ (:y pos) half-block)))
      :texture (-> entity (assoc :x (+ (:x pos) block-size)
                                 :y (+ (:y pos) block-size))))))

(defn move-agent [entities pair]
  (map
    (fn [e]
      (if-let [_ (:player? e)]
        (do-trans! e pair)
        e)) entities))

(def choice-observer (observe! policy-channel))

(defn show-choice [entities]
  (if-let [updated (choice-observer #(move-agent entities %))]
    updated
    entities))