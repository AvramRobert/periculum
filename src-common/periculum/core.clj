(ns periculum.core
  (:use [periculum.play]
        [periculum.animated]
        [periculum.more])
  (:require [play-clj.core :refer :all]
            [play-clj.ui :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [clojure.core.match :refer [match]]
            [clj-tuple :as t]
            [periculum.domain :refer [->State]]
            [periculum.world :refer [->Pos]]))

(defn assoc-from [entity txture]
  (let [pos (block-pos (:position entity))]
    (assoc txture
      :x (:x pos)
      :y (:y pos)
      :width block-size
      :height block-size)))

(defn p-ext [f entities]
  (filter (fn [e]
            (not (some
                   #(and (f % e)
                         (= (-> % :position :y) (-> e :position :y))) entities))) entities))

(defn dist-floor [left-texture
                  right-texture
                  rest-texture
                  entities]
  (let [lefts (p-ext #(= (-> %1 :position :x) (- (-> %2 :position :x) 1)) entities)
        rights (p-ext #(= (-> %1 :position :x) (+ 1 (-> %2 :position :x))) entities)
        rests (clojure.set/difference (set entities) (set lefts) (set rights))]
    (flatten
      [(map #(assoc-from % left-texture) lefts)
       (map #(assoc-from % right-texture) rights)
       (map #(assoc-from % rest-texture) rests)])))

(defn wall-shape [entity]
  (let [pos (to-render-pos entity)]
    (assoc (shape :filled
                  :set-color (color :coral)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn dist-wall [t-left-texture
                 t-right-texture
                 b-left-texture
                 b-right-texture
                 r-left-texture
                 r-right-texture
                 rest-texture
                 entities]
  (let [top (max-by #(-> % :position :y) entities)
        bottom (min-by #(-> % :position :x) entities)
        lefts (->> entities
                   (filter #(= (-> % :position :y) (-> top :position :y)))
                   (dist-floor t-left-texture t-right-texture rest-texture))
        rights (->> entities
                    (filter #(= (-> % :position :y) (-> bottom :position :y)))
                    (dist-floor b-left-texture b-right-texture rest-texture))
        rests (->> entities
                   (filter
                     #(and
                       (not= (-> % :position :y) (-> top :position :y))
                       (not= (-> % :position :y) (-> bottom :position :y))))
                   (dist-floor r-left-texture r-right-texture rest-texture))]
    (flatten
      [lefts rights rests])))

(defn wall-texture [tiles entities]
  (if (not-empty? entities)
    (dist-wall
      (texture (aget tiles 2 8))                            ;;top left corner
      (texture (aget tiles 2 10))                           ;;top right corner
      (texture (aget tiles 2 8))                            ;;bottom left corner
      (texture (aget tiles 2 10))                           ;;bottom right corner
      (texture (aget tiles 2 8))                            ;;inner left corner
      (texture (aget tiles 2 10))                           ;;inner right corner
      (texture (aget tiles 2 9))                            ;;inner
      entities)
    (t/tuple)))

(defn floor-shape [entity]
  (let [pos (to-render-pos entity)]
    (assoc (shape :filled
                  :set-color (color :gray)
                  :rect 0 0 block-size block-size) :x (:x pos) :y (:y pos))))

(defn floor-texture [tiles entities]
  (if (not-empty? entities)
    (dist-floor
      (texture (aget tiles 1 0))                            ;;left corner
      (texture (aget tiles 1 2))                            ;;right corner
      (texture (aget tiles 1 1))                            ;;inner
      entities)
    (t/tuple)))

(defn agent-shape [x y]
  (let [pos (block-pos x y)]
    (assoc (shape :filled
                  :set-color (color :scarlet)
                  :rect 0 0 block-size block-size)
      :x (:x pos)
      :y (:y pos)
      :animated? false)))

(defn fly [entities dir]
  (on-player
    entities
    (fn [agent-tex]
      (let [norm (normalise (:x agent-tex) (:y agent-tex))
            [nx ny] (t/tuple (:x norm) (+ (:y norm) dir))
            pos (block-pos (:x norm) (+ (:y norm) dir))]
        (assoc agent-tex
          :x (:x pos)
          :y (:y pos)
          :state (->State (->Pos nx ny) (-> agent-tex :state :previous-action)))))))

(defn agent-texture [sheet width height]
  (let [tiles (texture! sheet :split width height)]
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
                   player-anim (agent-texture animation-sheet texture-size texture-size)
                   environment-sheet (texture "env-tiles1.png")
                   env-tiles (texture! environment-sheet :split texture-size texture-size)
                   landscape (assoc (texture "wall1.png")
                               :id :background
                               :width (:width screen)
                               :height (:height screen))
                   walls (->> world
                              (filter #(= (:type %) :wall))
                              (wall-texture env-tiles)
                              (flatten))
                   floors (->> world
                               (filter #(= (:type %) :floor))
                               (floor-texture env-tiles)
                               (flatten))
                   player (player-anim (-> start :position :x) (-> start :position :y))]
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
                 (do-act entities :run-right)
                 (= key (key-code :u))
                 (fly entities 1)))))

(defgame periculum-game
         :on-create
         (fn [this]
           (set-screen! this sprite-screen)))
