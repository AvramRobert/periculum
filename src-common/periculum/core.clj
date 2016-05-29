(ns periculum.core
  (:use [periculum.play]
        [periculum.animated])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [periculum.world :refer [world-from-pixmap]]
            [clojure.core.match :refer [match]]))

(defn wall-shape [entity]
  (let [pos (to-render-pos entity)]
    (assoc (shape :filled
                  :set-color (color :coral)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn wall-texture [entity]
  (let [pos (to-render-pos entity)]
    (assoc (texture "cobblestone.png")
      :x (:x pos)
      :y (:y pos)
      :width block-size
      :height block-size)))

(defn floor-shape [entity]
  (let [pos (to-render-pos entity)]
    (assoc (shape :filled
                  :set-color (color :gray)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn floor-texture [entity]
  (let [pos (to-render-pos entity)]
    (assoc (texture "grass_side.png")
      :x (:x pos)
      :y (:y pos)
      :width block-size
      :height block-size)))

(defn agent-shape [x y]
  (let [pos (block-pos x y)]
    (assoc (shape :filled
                  :set-color (color :scarlet)
                  :rect 0 0 block-size block-size)
      :x (:x pos)
      :y (:y pos)
      :animated? false)))

(defn agent-texture [sheet]
  (let [tiles (texture! sheet :split half-block half-block)]
    (fn [x y]
      (let [pos (block-pos x y)]
        (assoc (texture (aget tiles 0 0))
          :x (:x pos)
          :y (:y pos)
          :width block-size
          :height block-size
          :animated? true
          :tiles tiles)))))

(def resize
  (fn [screen entities]
    (height! screen (:height screen))
    (on-background entities
                   #(assoc % :height (:height screen)
                             :width (:width screen)))))

(def rendering
  (fn [screen entities]
    (clear!)
    (let [do-act (comp apply-action supply-action)]
      (->> entities
           (do-act)
           (render! screen)))))

(declare periculum-game sprite-screen simple-screen)

(defscreen simple-screen
           :on-show
           (fn [screen entities]
             (update! screen :renderer (stage) :camera (orthographic))
             (let [walls (->> world
                              (filter #(= (:type %) :wall))
                              (map wall-shape))
                   floors (->> world
                               (filter #(= (:type %) :floor))
                               (map floor-shape))
                   player (agent-shape 1 1)]
               (flatten [(map #(assoc % :wall? true) walls)
                         (map #(assoc % :floor? true) floors)
                         [(player-entity player)]])))

           :on-resize resize
           :on-render rendering
           :on-key-down
           (fn [screen entities]
             (let [key (:key screen)
                   send! (send-record! periculum.play/expect-channel)
                   do-act (comp apply-action supply-action)
                   do-send (comp send! stop-record-path)]
               (cond
                 (= key (key-code :minus))
                 (do-send entities)
                 (= key (key-code :plus))
                 (record-path entities)
                 (= key (key-code :dpad-down))
                 (do-act entities :stand)
                 (= key (key-code :dpad-up))
                 (do-act entities :jump-up)
                 (= key (key-code :dpad-left))
                 (do-act entities :walk-left)
                 (= key (key-code :dpad-right))
                 (do-act entities :walk-right)
                 (= key (key-code :q))
                 (do-act entities :jump-left)
                 (= key (key-code :e))
                 (do-act entities :jump-right)
                 (= key (key-code :z))
                 (do-act entities :run-jump-left)
                 (= key (key-code :c))
                 (do-act entities :run-jump-right)
                 (= key (key-code :a))
                 (do-act entities :run-left)
                 (= key (key-code :d))
                 (do-act entities :run-right)))))

(defscreen sprite-screen
           :on-show
           (fn [screen entities]
             (update! screen :renderer (stage) :camera (orthographic))
             (let [animation-sheet (texture "sheet.png")
                   player-anim (agent-texture animation-sheet)
                   landscape (assoc (texture "wall1.jpg")
                               :id :background
                               :width (:width screen)
                               :height (:height screen))
                   walls (->> world
                              (filter #(= (:type %) :wall))
                              (map wall-texture))
                   floors (->> world
                               (filter #(= (:type %) :floor))
                               (map floor-texture))
                   player (player-anim 1 1)]
               (flatten [[landscape]
                         (map #(assoc % :wall? true) walls)
                         (map #(assoc % :floor? true) floors)
                         [(player-entity player)]])))

           :on-resize resize
           :on-render rendering
           :on-key-down
           (fn [screen entities]
             (let [key (:key screen)
                   send! (send-record! periculum.play/expect-channel)
                   do-act (comp apply-action supply-action)
                   do-send (comp send! stop-record-path)]
               (cond
                 (= key (key-code :minus))
                 (do-send entities)
                 (= key (key-code :plus))
                 (record-path entities)
                 (= key (key-code :dpad-down))
                 (do-act entities :stand)
                 (= key (key-code :dpad-up))
                 (do-act entities :jump-up)
                 (= key (key-code :dpad-left))
                 (do-act entities :walk-left)
                 (= key (key-code :dpad-right))
                 (do-act entities :walk-right)
                 (= key (key-code :q))
                 (do-act entities :jump-left)
                 (= key (key-code :e))
                 (do-act entities :jump-right)
                 (= key (key-code :z))
                 (do-act entities :run-jump-left)
                 (= key (key-code :c))
                 (do-act entities :run-jump-right)
                 (= key (key-code :a))
                 (do-act entities :run-left)
                 (= key (key-code :d))
                 (do-act entities :run-right)))))

(defgame periculum-game
         :on-create
         (fn [this]
           (set-screen! this sprite-screen)))
