(ns periculum.animated
  (:use [periculum.domain]
        [periculum.world]
        [periculum.more])
  (:require
    [clojure.core.async :as async]
    [clojure.core.match :refer [match]]
    [play-clj.math :as gmath]
    [play-clj.g2d :refer [texture texture!]]
    [periculum.play :as play]
    [periculum.rl :as rl]))

(def ^:const delta (/ 1 20))
(def ^:const fpa 5)
(def ^:const scale 1/2)
(def ^:const tile-size 32)
(def ^:const texture-size 32)

(def block-size (* scale tile-size))

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

(defn normalise
  ([x y]
   (->Pos (/ x block-size) (/ y block-size)))
  ([position]
   (->Pos (/ (:x position) block-size) (/ (:y position) block-size))))

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
  (let [Ω (omega play/world primitive-actions)
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

(defn when-recorded [entity f]
  (if (:record? entity)
    (f entity)
    entity))

(defn append-to-path [e]
  (assoc e
    :recorded-path (conj (:recorded-path e)
                         (rl/->Pair (:state e) (:current-action e)))))

(defn apply-inc [entity action-k]
  (let [f (:interpolator entity)
        t' (+ (:t entity) delta)
        pos' (apply-inter f t')]
    (if (>= (:t entity) 1.0)
      (-> entity
          (when-recorded append-to-path)
          (assoc
            :state (assoc (:state entity) :position (:correction entity)
                                          :previous-action action-k)
            :x (-> entity :correction block-pos :x)
            :y (-> entity :correction block-pos :y)
            :t 0.0
            :completed? true
            :current-action :stand))
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
  (let [path (path (:state entity) action)
        control-points (->> path
                            (pick-points)
                            (map #(->Pos (* (-> % :position :x) block-size)
                                         (* (-> % :position :y) block-size)))
                            (map pos-to-vec))]
    (assoc entity
      :current-action action
      :correction (-> path last :position)
      :completed? false
      :interpolator (gmath/bezier control-points))))

(defn play-sequence [entity actions]
  (if (empty? actions)
    (dissoc entity :sequence)
    (-> entity
        (assoc :sequence (drop 1 actions))
        (select-action (->> actions (first) :action)))))

(defn attempt-next [entity]
  (if-let [actions (:sequence entity)]
    (play-sequence entity actions)
    (if-let [chain (async/poll! play/result-channel)]
      (attempt-next (assoc entity :sequence chain))
      entity)))

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
      :record? false
      :completed? true
      :correction position
      :state (->State position :stand)
      :current-action :stand
      :t 0.0)))