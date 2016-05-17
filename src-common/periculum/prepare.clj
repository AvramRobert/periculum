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
(def worst-bound -1000.0)
(def best-bound 0.0)

(def policy-channel (async/chan (async/buffer 2048)))
(def qs-channel (async/chan (async/buffer 2048)))

(defn state [x y action]
  (->State (->Pos x y) action))

(def world-config {:floor     (m-struct 16 0 (pos 0 0))
                   :holes     [(pos 3 0) (pos 4 0) (pos 9 0) (pos 10 0)]
                   :walls     [(m-struct 2 3 (pos 7 1))]
                   :platforms [(m-struct 2 (pos 10 5))]})

;(def world-config {:floor     (m-struct 14 0 (pos 0 0))
;                   :holes     [(pos 3 0) (pos 13 0) (pos 14 0) (pos 15 0)]
;                   :walls     [(m-struct 2 3 (pos 5 1)) (m-struct 2 3 (pos 10 1))]
;                   :platforms empty-vec
;                   })

(def world (make-world world-config))

(defn t [value]
  (Math/abs ^Double (/ value worst-bound)))

(defn choice-supplier [algorithm]
  (let [data (conf 1.0 0.4 0.7)
        policy (eps-greedy 0.6 greedy-by-max policy-channel)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm policy action-f reward-f transition-f terminal-f)]
    (control exp-algorithm data)))

(defn qs-supplier [algorithm]
  (let [data (conf 1.0 0.4 0.7)
        policy (eps-greedy 0.6 greedy-by-max)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm qs-channel policy action-f reward-f transition-f terminal-f)]
    (control exp-algorithm data)))

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
        e-type (:e-type entity)]
    (case e-type
      :geometric (-> entity (assoc :x (+ (:x pos) half-block) :y (+ (:y pos) half-block)))
      :texture (-> entity (assoc :x (* (:x pos) block-size) :y (* (:y pos) block-size))))))

(defn move-agent [entities pair]
  (map
    (fn [e]
      (if-let [_ (:player? e)]
        (do-trans! e pair)
        e)) entities))

(defn adapt-values [entities data]
  (map
    (fn [[S As]]
      (let [x (-> S :position :x)
            y (-> S :position :y)
            lower (color :red)
            upper (color :green)
            mean (action-mean As)
            n-color (color! lower :lerp upper (t mean))]
        (assoc
          (shape :filled
                 :set-color n-color
                 :rect 0 0 block-size block-size)
          :x (* x block-size) :y (* y block-size)))) (:q-values data)))

(def choice-observer (observe! policy-channel))
(def q-values-observer (observe! qs-channel))

(defn show-choice [entities]
  (if-let [updated (choice-observer #(move-agent entities %))]
    updated
    entities))

(defn show-qs [entities]
  (if-let [values (q-values-observer #(adapt-values entities %))]
    values
    entities))