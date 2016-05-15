(ns periculum.core
  (:use [periculum.prepare])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [periculum.world :refer [make-world m-struct pos]]
            [clojure.core.match :refer [match]]))

  (def config {:floor     (m-struct 11 0 (pos 0 0))
               :holes     [(pos 4 0) (pos 5 0)]
               :walls     [(m-struct 2 3 (pos 2 1))]
               :platforms [(m-struct 3 (pos 4 5))]})

(defn derive-pos
  [entity]
  (let [x (get-in entity [:position :x])
        y (get-in entity [:position :y])]
    (block-pos x y)))

(defn wall-shape [entity]
  (let [pos (derive-pos entity)]
    (shape :filled
           :set-color (color :red)
           :rect (:x pos) (:y pos) block-size block-size)))

(defn floor-shape [entity]
  (let [pos (derive-pos entity)]
    (shape :filled
           :set-color (color :green)
           :rect (:x pos) (:y pos) block-size block-size)))

(defn player-shape [x y]
  (let [pos (block-pos x y)]
    (assoc (shape :filled
                  :set-color (color :white)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(def check! (observe! policy-channel))

(defn look-at [entities]
  (if-let [new-nts (check! #(move-test entities %))]
    new-nts
    entities))

(defn pause [amount entities]
  (Thread/sleep amount)
  entities)

(defscreen main-screen
           :on-show
           (fn [screen entities]
             (update! screen :renderer (stage) :camera (orthographic))
             (let [env (make-world config)
                   walls (->> env
                              (filter #(= (:type %) :wall))
                              (map wall-shape))
                   floors (->> env
                               (filter #(= (:type %) :floor))
                               (map floor-shape))
                   player (player-shape 0 1)]
               (flatten [
                         (map #(assoc % :wall? true) walls)
                         (map #(assoc % :floor? true) floors)
                         [(assoc player :player? true)]])))

           :on-resize
           (fn [screen entities]
             (height! screen 600))

           :on-render
           (fn [screen entities]
             (clear!)
             (->> entities
                  (look-at)
                  (pause 100)
                  (render! screen))))

(defgame periculum-game
         :on-create
         (fn [this]
           (set-screen! this main-screen)))
