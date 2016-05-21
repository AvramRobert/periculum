(ns periculum.core
  (:use [periculum.prepare])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [periculum.world :refer [make-world m-struct pos]]
            [clojure.core.match :refer [match]]
            [periculum.animated :as anim]))

(defn wall-shape [entity]
  (let [pos (derive-pos entity)]
    (assoc (shape :filled
                  :set-color (color :coral)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn wall-texture [entity]
  (let [pos (derive-pos entity)]
    (assoc (texture "brick.png")
      :x (:x pos) :y (:y pos) :width block-size :height block-size)))

(defn floor-shape [entity]
  (let [pos (derive-pos entity)]
    (assoc (shape :filled
                  :set-color (color :gray)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn floor-texture [entity]
  (let [pos (derive-pos entity)]
    (assoc (texture "grass_side.png")
      :x (:x pos) :y (:y pos) :width block-size :height block-size)))

(defn agent-shape [x y]
  (let [pos (block-pos x y)]
    (assoc (shape :filled
                  :set-color (color :scarlet)
                  :circle 0 0 half-block) :x (+ (:x pos) half-block) :y (+ (:y pos) half-block))))

(defn agent-shape2 [x y]
  (let [pos (block-pos x y)]
    (assoc (shape :filled
                  :set-color (color :scarlet)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn agent-texture [x y]
  (let [pos (block-pos x y)]
    (assoc (texture "krabby.png")
      :x (:x pos) :y (:y pos) :width block-size :height block-size)))

(defn pause [amount entities]
  (Thread/sleep amount)
  entities)

(def resize
  (fn [screen entities]
    (height! screen 600)))

(declare periculum-game sprite-screen simple-screen)

(defn update-fps [entities]
  (map (fn [e]
         (case (:id e)
           :fps (doto e (label! :set-text (str (game :fps))))
           e)) entities))

(defscreen simple-screen
           :on-show
           (fn [screen entities]
             (update! screen :renderer (stage) :camera (orthographic))
             (let [env (make-world world-config)
                   walls (->> env
                              (filter #(= (:type %) :wall))
                              (map wall-shape))
                   floors (->> env
                               (filter #(= (:type %) :floor))
                               (map floor-shape))
                   player (agent-shape2 1 1)]
               (flatten [(map #(assoc % :wall? true) walls)
                         (map #(assoc % :floor? true) floors)
                         [(anim/player-entity player :texture)]])))

           :on-resize resize

           :on-render
           (fn [screen entities]
             (clear!)
             (let [do-act (comp anim/apply-action anim/supply-action)]
               (->> entities
                    ;(pause 50)
                    ;(update-fps)
                    ;(show-choice)
                    ;(do-act)
                    (anim/apply-action)
                    (render! screen))))

           :on-key-down
           (fn [screen entities]
             (let [key (:key screen)
                   do-act (comp anim/apply-action anim/supply-action)]
               (cond
                 (= key (key-code :dpad-down))
                 (do-act entities :stand)
                 (= key (key-code :dpad-up))
                 (do-act entities :jump)
                 (= key (key-code :dpad-left))
                 (do-act entities :walk-left)
                 (= key (key-code :dpad-right))
                 (do-act entities :walk-right)))))

(defscreen sprite-screen
           :on-show
           (fn [screen entities]
             (update! screen :renderer (stage) :camera (orthographic))
             (let [env (make-world world-config)
                   landscape (assoc (texture "wall1.jpg")
                               :width (game :width) :height (game :height))
                   walls (->> env
                              (filter #(= (:type %) :wall))
                              (map wall-texture))
                   floors (->> env
                               (filter #(= (:type %) :floor))
                               (map floor-texture))
                   player (agent-texture 1 1)]
               (flatten [[landscape]
                         (map #(assoc % :wall? true) walls)
                         (map #(assoc % :floor? true) floors)
                         [(anim/player-entity player :texture)]])))

           :on-resize resize

           :on-render
           (fn [screen entities]
             (clear!)
             (->> entities
                  (pause 100)
                  (show-choice)
                  (render! screen))))

(defgame periculum-game
         :on-create
         (fn [this]
           (set-screen! this simple-screen)))
