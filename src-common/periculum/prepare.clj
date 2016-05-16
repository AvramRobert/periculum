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

(def world-conf {:floor     (m-struct 11 0 (pos 0 0))
                 :holes     [(pos 4 0) (pos 5 0)]
                 :walls     [(m-struct 2 3 (pos 2 1))]
                 :platforms [(m-struct 3 (pos 4 5))]})

(defn block-pos
  ([x y]
   (pos (* x block-size) (* y block-size)))
  ([{x :x y :y}]
   (block-pos x y)))

(defn observe! [channel]
  (delayed-observer channel))

(defn do-trans! [entity state]
  (let [pos (:position state)]
    (-> entity (assoc :x (:x pos)) (assoc :y (:y pos)))))

(defn move-agent [entities state]
  (map
    (fn [e]
      (if-let [_ (:player? e)]
        (do-trans! e state)
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
