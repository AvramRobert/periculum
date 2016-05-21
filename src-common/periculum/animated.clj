(ns periculum.animated
  (:use [periculum.learning]
        [periculum.world]
        [periculum.more])
  (:require
    [clojure.core.async :as async]
    [clojure.core.match :refer [match]]
    [play-clj.math :as gmath]
    [periculum.prepare :as p]))

(def ^:const delta (/ 1 20))

(defn normalise [x y]
  (->Pos (/ x p/block-size) (/ y p/block-size)))

;; FIXME: Look at descension again. Block sometimes stops next to solid
;; It seems that, in some cases, the descension makes the block stop in the air, next to the actual solid.
;; I believe it's because it sees that next to it, there is a solid on which it can stand, but
;; stops right before reaching it. Perhaps due to `take-while`

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
      (assoc entity
        :t t'
        :x (:x pos')
        :y (:y pos')))))

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
                       (map #(->Pos (* (-> % :position :x) p/block-size)
                                    (* (-> % :position :y) p/block-size)))
                       (map pos-to-vec))]
    (assoc entity
      :current-action action
      :completed? false
      :interpolator (gmath/bezier real-path))))

(defn attempt-next [entity]
  (let [f (or-else #(select-action entity (:action %)) entity)]
    (f (async/poll! p/command-chan))))

(defn supply-action
  ([entities]
   (on-player entities #(when-complete % attempt-next)))
  ([entities action]
   (on-player entities (fn [e]
                         (when-complete e #(select-action % action))))))

(defn player-entity [shape body]
  (let [position (case body
                   :geometric (normalise (- (:x shape) p/half-block) (- (:y shape) p/half-block))
                   :texture (normalise (:x shape) (:y shape)))]
    (assoc shape
      :id :player
      :body body
      :completed? true
      :state (->State position :stand)
      :current-action :stand
      :t 0.0)))





