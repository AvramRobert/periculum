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

(def world {:floor     (m-struct 11 0 (pos 0 0))
            :holes     [(pos 4 0) (pos 5 0)]
            :walls     [(m-struct 2 3 (pos 2 1))]
            :platforms [(m-struct 3 (pos 4 5))]})


(defn terminal? [world]
  (let [max (max-by #(-> % (:position) (:x)) world)]
    (fn [state]
      (= (-> state (:position) (:x)) (-> max (:position) :x)))))

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

(defn move! [entity [x y] stride]
  (if-let [_ (:player? entity)]
    (-> entity (update :x #(+ % (* x stride))) (update :y #(+ % (* y stride))))
    entity))

(defn move-test [entities state]
  (map
    (fn [e]
      (if-let [_ (:player? e)]
        (move! e state block-size)
        e)) entities))

(defn to-left [channel]
  (async/thread
    (println "Sending..")
    (reduce
      (fn [l _]
        (async/>!! channel [1 0])
        l) 0 (range 0 10))))
