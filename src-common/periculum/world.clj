(ns periculum.world
  (use clojure.set)
  (use [periculum.coll.more :only [empty-vec]]))

(def shapes '(:rectangle :circle))

(defrecord Pos [x y])

(defrecord Struct [length
                   height
                   start-pos])

(defrecord Entity [label
                   shape
                   position
                   components
                   solid?])

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

(defn solidify [position]
  (->Entity "Solid" :rectangle position empty-vec true))

(defn solidify-many [lattice]
  (map solidify lattice))

(defn platform [length holes]
  (fn [start-pos]
    (let [lattice ((lattice length 1) start-pos)
          holeless (difference (set lattice) (set holes))]
      (solidify-many holeless))))

(defn make-solid [length height]
  (fn
    ([start-pos]
     (solidify-many ((lattice length height) start-pos)))
    ([x y]
     (solidify-many ((lattice length height) (->Pos x y))))))

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

(comment
  (def world-conf {:floor     <length>
                   :holes     [<pos>]
                   :walls     [<length, height, pos>]
                   :platforms [<length, pos>]
                   }))

; FIXME: Write macro for world creation
; FIXME: It would be quite practical to have something like `when-all`, which only executes the block when `all` bindings are non-nil
; The world itself has a width and a length, which I should take into account somewhere, because that is how I know its bounds
(defn make-world [config]
  (let [fl ((make-platform (:floor config) (:holes config)) 0 0)
        walls (map #((make-solid (:length %) (:height %)) (:start-pos %)) (:walls config))
        platforms (map #((make-platform (:length %) []) (:start-pos %)) (:platforms config))]
    (flatten (into (into fl walls) platforms))))