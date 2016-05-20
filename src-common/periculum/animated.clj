(ns periculum.animated
  (:use [periculum.learning]
        [periculum.world]
        [periculum.more])
  (:require
    [clojure.core.async :as async]
    [clojure.core.match :refer [match]]
    [play-clj.math :as gmath]
    [periculum.prepare :as p])
  (:import (com.badlogic.gdx.math Vector2)))

(def ^:const delta (/ 1 20))
(def command-chan (async/chan))

(defn normalise [x y]
  (->Pos (/ x p/block-size) (/ y p/block-size)))

;; FIXME: Look at descension again. Block sometimes stops next to solid
;; It seems that, in some cases, the descension makes the block stop in the air, next to the actual solid.
;; I believe it's because it sees that next to it, there is a solid on which it can stand, but
;; stops right before reaching it. Perhaps due to `take-while`

;; FIXME: Add the following logic to the physics: After jumping, the previous action is reset to :stand

;; FIXME: pick 4 points instead, and make them more meaningful
;; We pick because libGDX only allows Bezier curves of 1, 2 or 3rd degree
(defn pick-points [points]
  [(first points) (mid points) (last points)])

(defn norm-round [x y]
  (let [norm (normalise x y)]
    (assoc norm
      :x (Math/round ^Float (:x norm))
      :y (Math/round ^Float (:y norm)))))

(def path (omega p/world all-actions))

(defn apply-inc [entity action-k]
  (let [f (:interpolator entity)
        t' (+ (:t entity) delta)
        pos' (gmath/bezier! f :value-at (new Vector2) t')]
    (if (>= (:t entity) 1.0)
      (assoc entity
        :state (assoc (:state entity)
                 :position (norm-round (.x pos') (.y pos'))
                 :previous-action (if (= action-k :jump)
                                    :stand
                                    action-k))
        :t 0.0
        :completed? true
        :current-action :stand)
      (assoc entity
        :t t'
        :x (.x pos')
        :y (.y pos')))))

(defn continue-action [entity action]
  (if-let [current (:current-action entity)]
    (if (= current action)
      (if (:completed? entity)
        entity
        (apply-inc entity action))
      (apply-inc entity action))))

(defn apply-action
  [entities]
  (map (fn [e]
         (match [(:id e)]
                [:player] (continue-action e (:current-action e))
                :else e)) entities))

(defn select-action [entities action]
  (map (fn [e]
         (match [(:id e)]
                [:player] (let [[_ all-states] (path (:state e) action)
                                picked (pick-points all-states)
                                real-path (map #(assoc %
                                                 :x (* (-> % :position :x) p/block-size)
                                                 :y (* (-> % :position :y) p/block-size))
                                               picked)]
                            (assoc e
                              :current-action action
                              :completed? false
                              :interpolator (gmath/bezier (map pos-to-vec real-path))
                              :t 0.0))
                :else e)) entities))


(defn player-entity [shape body]
  (let [position (case body
                   :geometric (normalise (- (:x shape) p/half-block) (- (:y shape) p/half-block))
                   :texture (normalise (:x shape) (:y shape)))]
    (assoc shape
      :id :player
      :body body
      :completed? true
      :state (->State position :stand)
      :current-action :stand)))





