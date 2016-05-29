(ns periculum.world
  (use clojure.set)
  (use [periculum.more :only [empty-vec]])
  (:require [play-clj.core :refer [color color!]]
            [mikera.image.colours :as clrs]
            [mikera.image.core :as imgs]))

(defrecord Pos [x y])

(defrecord Struct [length
                   height
                   start-pos])

(defrecord Entity [label
                   type
                   position
                   components
                   solid?])

;; Floor: ffff00ff ;; ff6600
;; Wall:  ff0000ff ;; ff0000

(comment
  (def world-conf {:floor     [<length, pos>]
                   :holes     [<pos>]
                   :walls     [<length, height, pos>]
                   :platforms [<length, pos>]
                   ;:other     [<pos> <pos> <pos>]           ; other things that can move
                   }))

(defn base [start length]
  (map #(->Pos (+ (:x start) %) (:y start)) (range 0 length)))

(defn lattice [length height]
  (fn [start-pos]
    (let [bases (repeat (base start-pos length))
          all (take height bases)]
      (flatten
        (map-indexed (fn [index items]
                       (map (fn [item]
                              (update item :y #(+ % index))) items)) all)))))

(defn solidify
  ([position]
   (->Entity "Solid" :none position empty-vec true))
  ([position ent-type]
   (->Entity "Solid" ent-type position empty-vec true)))

(defn solidify-many
  ([lattice]
   (map solidify lattice))
  ([lattice ent-type]
   (map #(solidify % ent-type) lattice)))

(defn platform [length holes]
  (fn [start-pos]
    (let [lattice ((lattice length 1) start-pos)
          holeless (difference (set lattice) (set holes))]
      (solidify-many holeless :floor))))

(defn make-solid [length height]
  (fn
    ([start-pos]
     (solidify-many ((lattice length height) start-pos) :wall))
    ([x y]
     (solidify-many ((lattice length height) (->Pos x y)) :wall))))

(defn make-platform [length holes]
  (fn
    ([start-pos]
     ((platform length holes) start-pos))
    ([x y]
     ((platform length holes) (->Pos x y)))))


(defn pos [x y]
  (->Pos x y))

(defn m-struct
  ([length height pos]
   (->Struct length height pos))
  ([length pos]
   (->Struct length 1 pos)))

(defn- img->entities [image]
  (let [width (imgs/width image)
        height (imgs/height image)]
    (for [x (range 0 width)
          y (range 0 height)]
      (let [primal (-> image
                       (imgs/get-pixel x y)
                       (clrs/color))
            clr (color (.getRed primal)
                       (.getGreen primal)
                       (.getBlue primal)
                       (.getAlpha primal))]
        (case (color! clr :to-string)
          "ffff00ff" (solidify (pos x (- height 1 y)) :floor)
          "ff0000ff" (solidify (pos x (- height 1 y)) :wall)
          :none)))))

(defn world-from-pixmap [path]
  (let [image (imgs/load-image path)]
    (->> image
         (img->entities)
         (filter #(not= % :none)))))

(defn make-world [config]
  (let [{floor-conf     :floor
         holes-conf     :holes
         walls-conf     :walls
         platforms-conf :platforms
         } config
        floor ((make-platform (:length floor-conf) holes-conf) (:start-pos floor-conf))
        walls (map #((make-solid (:length %) (:height %)) (:start-pos %)) walls-conf)
        platforms (map #((make-platform (:length %) empty-vec) (:start-pos %)) platforms-conf)]
    (flatten [floor walls platforms])))