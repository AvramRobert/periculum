(ns periculum.animated
  (:use [periculum.learning]
        [periculum.world]
        [periculum.more])
  (:require
    [clojure.core.async :as async]
    [clojure.core.match :refer [match]]
    [play-clj.math :as gmath]
    [play-clj.g2d :refer [texture texture!]]
    [periculum.dsl :as p]))

(def ^:const delta (/ 1 20))
(def ^:const fpa 5)
(def ^:const scale 2)
(def ^:const tile-size 32)

(def block-size (* scale tile-size))
(def half-block (/ block-size 2))

(defn anim-idx-for [action]
  (case action
    :stand 6
    :walk-left 3
    :walk-right 2
    :run-left 1
    :run-right 0
    :jump-up 4
    :jump-left 5
    :jump-right 4
    :run-jump-left 5
    :run-jump-right 4
    6))

(defn block-pos
  ([x y]
   (pos (* x block-size) (* y block-size)))
  ([{x :x y :y}]
   (block-pos x y)))


(defn to-render-pos [world-entity]
  (let [x (get-in world-entity [:position :x])
        y (get-in world-entity [:position :y])]
    (block-pos x y)))

(defn normalise [x y]
  (->Pos (/ x block-size) (/ y block-size)))

(defn frame-at [t]
  (-> t (* fpa) (Math/floor) (Math/round)))

;; FIXME: pick 4 points instead, and make them more meaningful
;; We pick because libGDX only allows Bezier curves of 1, 2 or 3rd degree
(defn pick-points [points]
  [(first points) (mid points) (last points)])

(defn norm-round
  ([x y]
   (let [norm (normalise x y)]
     (assoc norm
       :x (Math/round ^Float (:x norm))
       :y (Math/round ^Float (:y norm)))))
  ([position]
   (norm-round (:x position) (:y position))))

(defn path [state action]
  (let [Ω (omega p/world primitive-actions)
        [_ p] (Ω state action)]
    p))

;; Complete agent animation

(defn animate [entity t]
  (if (:animated? entity)
    (let [step (frame-at t)
          idx (anim-idx-for (:current-action entity))]
      (texture! entity :set-region (aget (:tiles entity) idx step))))
  entity)

(defn apply-inter [interpolant t]
  (let [vec-2 (gmath/bezier! interpolant :value-at (gmath/vector-2*) t)]
    (->Pos (.x vec-2)
           (.y vec-2))))

(defn apply-inc [entity action-k]
  (let [f (:interpolator entity)
        t' (+ (:t entity) delta)
        pos' (apply-inter f t')]
    (if (>= (:t entity) 1.0)
      (assoc entity
        :state (assoc (:state entity) :position (norm-round pos')
                                      :previous-action action-k)
        :t 0.0
        :completed? true
        :current-action :stand)
      (-> entity (assoc :t t'
                        :x (:x pos')
                        :y (:y pos')) (animate t')))))

(defn when-complete [entity f]
  (if (:completed? entity)
    (f entity)
    entity))

(defn when-not-complete [entity f]
  (if (:completed? entity)
    entity
    (f entity)))

(defn on-player [entities f]
  (map (fn [e]
         (case (:id e)
           :player (f e)
           e)) entities))

(defn on-background [entities f]
  (map (fn [e]
         (case (:id e)
           :background (f e)
           e)) entities))

(defn continue-action [entity action]
  (if-let [current (:current-action entity)]
    (if (= current action)
      (when-not-complete entity #(apply-inc % action))
      (apply-inc entity action))))

(defn apply-action [entities]
  (on-player entities #(continue-action % (:current-action %))))

(defn select-action [entity action]
  (let [real-path (->> (path (:state entity) action)
                       (pick-points)
                       (map #(->Pos (* (-> % :position :x) block-size)
                                    (* (-> % :position :y) block-size)))
                       (map pos-to-vec))]
    (assoc entity
      :current-action action
      :completed? false
      :interpolator (gmath/bezier real-path))))

(defn attempt-next [entity]
  (let [f (or-else #(select-action entity (:action %)) entity)]
    (f (async/poll! p/result-channel))))

(defn supply-action
  ([entities]
   (on-player entities #(when-complete % attempt-next)))
  ([entities action]
   (on-player entities (fn [e]
                         (when-complete e #(select-action % action))))))

(defn player-entity [shape]
  (let [position (normalise (:x shape) (:y shape))]
    (assoc shape
      :id :player
      :completed? true
      :state (->State position :stand)
      :current-action :stand
      :t 0.0)))

;; Minimalistic agent transition display without animation

(defn do-trans! [entity pair]
  (let [pos (block-pos (->> pair (:state) (:position)))]
    (-> entity (assoc :x (+ (:x pos) block-size)
                      :y (+ (:y pos) block-size)))))

(defn move-agent [entities pair]
  (map
    (fn [e]
      (if-let [_ (:player? e)]
        (do-trans! e pair)
        e)) entities))

(defn show-choice [entities]
  (if-let [updated (p/choice-observer #(move-agent entities %))]
    updated
    entities))





