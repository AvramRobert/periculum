(ns periculum.prepare
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.learning]
    [periculum.more])
  (:require
    [clojure.core.async :as async]))

(def block-size 32)

(def policy-channel (async/chan))
(def qs-channel (async/chan))

(defn state [x y action]
  (->State (->Pos x y) action))

;(def world-config {:floor     (m-struct 11 0 (pos 0 0))
;                 :holes     [(pos 4 0) (pos 5 0)]
;                 :walls     [(m-struct 2 3 (pos 2 1))]
;                 :platforms [(m-struct 3 (pos 4 5))]})

(def world-config {:floor     (m-struct 15 0 (pos 0 0))
                   :holes     [(pos 3 0) (pos 13 0) (pos 14 0) (pos 15 0)]
                   :walls     [(m-struct 2 3 (pos 5 1)) (m-struct 2 3 (pos 10 1))]
                   :platforms empty-vec
                   })

(def world (make-world world-config))

(defn default [algorithm]
  (let [data (conf 1.0 0.4 0.7)
        policy (eps-greedy 0.6 greedy-by-max policy-channel)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm policy action-f reward-f transition-f terminal-f)]
    (control exp-algorithm data)))

(def tst
  (let [data (conf 1.0 0.4 0.7)
        policy (eps-greedy 0.6 greedy-by-max)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)]
    (lazy-traj policy action-f reward-f transition-f)))

(defn block-pos
  ([x y]
   (pos (* x block-size) (* y block-size)))
  ([{x :x y :y}]
   (block-pos x y)))

(defn observe! [channel]
  (delayed-observer channel))

(defn do-trans! [entity pair]
  (let [pos (->> pair (:state) (:position))]
    (-> entity (assoc :x (* (:x pos) block-size)) (assoc :y (* (:y pos) block-size)))))

(defn move-agent [entities pair]
  (map
    (fn [e]
      (if-let [_ (:player? e)]
        (do-trans! e pair)
        e)) entities))

;; FIXME: find a way to properly grade the colours based on q-values
(defn adapt-values [entities data pr-x]
  (map
    (fn [e]
      (let [x (:x e)
            y (:y e)
            values (find-some #(and
                                (= x (get-in % [:position :x]))
                                (= y (get-in % [:position :y]))) (:q-values data))
            probs (->> values (pr-x) (action-mean))])) entities))

(def choice-observer (observe! policy-channel))
(def q-values-observer (observe! qs-channel))

(defn show-choice [entities]
  (if-let [updated (choice-observer #(move-agent entities %))]
    updated
    entities))
