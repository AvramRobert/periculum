(ns periculum.core
  (:use [periculum.prepare])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [periculum.world :refer [make-world m-struct pos]]
            [clojure.core.match :refer [match]]))

(defn derive-pos
  [entity]
  (let [x (get-in entity [:position :x])
        y (get-in entity [:position :y])]
    (block-pos x y)))

(defn wall-shape [entity]
  (let [pos (derive-pos entity)]
    (assoc (shape :filled
                  :set-color (color :red)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn floor-shape [entity]
  (let [pos (derive-pos entity)]
    (assoc (shape :filled
                  :set-color (color :green)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn player-shape [x y]
  (let [pos (block-pos x y)]
    (assoc (shape :filled
                  :set-color (color :white)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn pause [amount entities]
  (Thread/sleep amount)
  entities)

(defn enlighten [entities]
  (let [ne (map
             (fn [e]
               (let [x (:x e)
                     y (:y e)]
                 (assoc (shape e
                               :set-color (color! (shape! e :get-color) :sub 0.015 0.015 0.0 0.0)
                               :rect 0 0 block-size block-size) :x x :y y)))
             entities)]
    ne))

(defscreen main-screen
           :on-show
           (fn [screen entities]
             (update! screen :renderer (stage) :camera (orthographic))
             (let [env (make-world world-conf)
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
                  (pause 100)
                  (enlighten)
                  (render! screen))))

(defgame periculum-game
         :on-create
         (fn [this]
           (set-screen! this main-screen)))
