(ns periculum.learning
  (:use [periculum.world])
  (:use [periculum.more])
  (:require [clj-tuple :as tuples])
  (:require [clojure.core.match :refer [match]])
  (:require [play-clj.math :as gmath])
  (import (com.badlogic.gdx.math Vector2)))

(defrecord Action [velocity
                   time
                   orientation])

(defrecord State-Complete [position
                           velocity
                           orientation])

(defrecord State [position
                  previous-action])

(defn gravity [h-max t-apex]
  (/ (* 2 h-max) (Math/pow t-apex 2)))

(defn jump-velocity [g-const h-max]
  (Math/sqrt (* 2 g-const h-max)))

(def H-max 4)
(def T-apex 2)
(def G (gravity H-max T-apex))
(def Rewards {:tic   -1
              :solid -5
              :end   20
              })

(def ^:private all-actions {:stand      (->Action 0 1 [0 0])
                            :walk-left  (->Action 1 1 [-1 0])
                            :walk-right (->Action 1 1 [1 0])
                            :run-left   (->Action 2 1 [-1 0])
                            :run-right  (->Action 2 1 [1 0])
                            :jump       (->Action (Math/round ^Float (jump-velocity G H-max)) T-apex [0 1])
                            :fall       (->Action G 1 [0 -1])
                            })

(defn update-pos [pos amount orient]
  (let [xed (assoc pos :x (+ (:x pos) (* amount (nth orient 0))))
        yed (assoc xed :y (+ (:y pos) (* amount (nth orient 1))))]
    yed))

(defn out?
  ([pos coord]
   (neg? (coord pos)))
  ([pos]
   (or (neg? (:x pos)) (neg? (:y pos)))))

(defn hole? [state]
  (zero? (:y (:position state))))

(defn solid? [pos lookup]
  (let [item (some #(:solid? %) (lookup pos))]
    (some? item)))

(defn empty-beneath? [cur-pos lookup]
  (let [beneath (pos (:x cur-pos) (dec (:y cur-pos)))]
    (or (not (solid? beneath lookup)) (pos? (:y beneath)))))

(defn solid-beneath? [cur-pos lookup]
  (let [beneath (pos (:x cur-pos) (dec (:y cur-pos)))]
    (or (solid? beneath lookup) (neg? (:y beneath)))))

(defn interval [actions]
  (let [maximum (->> actions (max-by :velocity) (:velocity))]
    (if (zero? maximum)
      1
      maximum)))

(defn interpolation [p1 p2]
  (fn [t]
    (let [nx (+ (:x p1) (* t (- (:x p2) (:x p1))))
          ny (+ (:y p1) (* t (- (:y p2) (:y p1))))]
      (pos (->> nx (Math/ceil) (Math/round))
           (->> ny (Math/ceil) (Math/round))))))

(defn +++ [t inter]
  (map #(inter %) (range 0.0 (+ 1.0 t) t)))

(defn endpoint [pos actions]
  (reduce (fn [npos action]
            (let [{velocity    :velocity
                   time        :time
                   orientation :orientation} action
                  step (Math/round ^float (/ velocity time))]
              (update-pos npos step orientation))) pos actions))

(defn pos-to-vec [pos]
  (gmath/vector-2 (double (:x pos)) (double (:y pos))))

(defn midpoint [start end]
  (let [x (-> (/ (+ (:x start) (:x end)) 2) (Math/floor) (Math/round))
        y (-> (/ (+ (:y start) (:y end)) 2) (Math/ceil) (Math/round))]
    (pos x y)))

(defn <+> [start actions]
  "Simple Linear interpolation"
  (let [t (/ 1 (interval actions))
        end (endpoint start actions)
        inter-f (interpolation start end)]
    (distinct (+++ t inter-f))))

(defn <++> [linear]
  "Bezier Cubic interpolation"
  (let [points [(-> linear (first) (pos-to-vec))
                (-> linear (mid) (pos-to-vec))
                (-> linear (last) (pos-to-vec))]
        spline (gmath/bezier points)
        values (for [t (range 0.0 1.0 0.1)]
                 (.valueAt spline (new Vector2) t))]
    (distinct
      (map (fn [v]
             (pos (Math/round (.x v)) (Math/round (.y v)))) values))))

(defn deref-actions [actions lookup]
  (map #(lookup %) actions))

(defn descend [lookup]
  (fn [pos actions]
    (loop [t 0
           cur pos
           visited (tuples/tuple)
           acts (conj actions :fall)]
      (let [todo (deref-actions acts lookup)
            interpolated (<+> cur todo)
            not-solid? #(and (not (solid-beneath? % lookup)) (not (out? % :y)))]
        (cond
          (some? (find-some #(out? % :x) interpolated))
          (let [valid (take-while #(or
                                    (pos? (:x %))
                                    (zero? (:x %))) interpolated)]
            (recur (inc t)
                   (->Pos 0 (-> valid (last-or cur) (:y)))
                   (into visited valid)
                   [:stand :fall]))

          (every? not-solid? (rest interpolated)) (recur (inc t)
                                                  (last interpolated)
                                                  (into visited (drop-last interpolated))
                                                  acts)
          :else (let [non-solid (take-while #(and (not (solid? % lookup))
                                                  (not (out? % :y))) interpolated)]
                  (tuples/tuple (inc t) (into visited non-solid)))
          )))))

(defn ascend [lookup]
  (fn [start other-acts]
    (let [T-apex (:time (lookup :jump))
          todo (deref-actions (conj other-acts :jump) lookup)]
      (reduce
        (fn [ps _]
          (let [path (<+> (last ps) todo)]
            (into ps (next path)))) (tuples/tuple start) (range 0 T-apex)))))

(defn fall
  ([pos lookup actions]
   ((descend lookup) pos actions))
  ([pos lookup]
   ((descend lookup) pos empty-vec)))

(defn move [lookup]
  (fn [pos action]
    (let [todo (deref-actions [action] lookup)
          interpolated (<+> pos todo)
          can-stand? #(solid-beneath? % lookup)]
      (if (every? can-stand? interpolated)
        (tuples/tuple 1 interpolated)
        (let [solid (vec (take-while can-stand? interpolated))
              non-solid (find-some #(not (can-stand? %)) interpolated)
              [t fallen] (fall non-solid lookup)]
          (tuples/tuple t (into solid fallen)))))))

(defn jump [lookup]
  (fn [pos other-action]
    (let [T-apex (:time (lookup :jump))
          other (tuples/tuple other-action)
          ascent ((ascend lookup) pos other)
          [t descent] (fall (last ascent) lookup other)]
      (tuples/tuple (+ t T-apex) (into ascent descent)))))


(defn stateify [time-position action]
  (let [[time ps] time-position]
    (tuples/tuple time (map #(->State % action) ps))
    ))

(defn eta-gen [world actions f]
  (fn [item]
    (if (keyword? item)
      (item actions)
      (f item world))))

(defn eta-one [world actions]
  (eta-gen world
           actions
           (fn [item -world]
             (find-some #(= (:position item) (:position %)) -world))))

(defn eta [world actions]
  (eta-gen world
           actions
           (fn [item -world]
             (filter #(= (:position item) (:position %)) -world))))


(defn eta-pos [world actions]
  (eta-gen world
           actions
           (fn [item -world]
             (filter #(= (:position %) item) -world))))

(defn omega [world actions]
  (fn [state action]
    (let [previous (:previous-action state)
          position (:position state)
          lookup (eta-pos world actions)
          call-move #(stateify ((move lookup) position %) %)]
      (match [action]
             [:jump] (stateify ((jump lookup) position previous) previous)
             [:walk-right] (call-move action)
             [:walk-left] (call-move action)
             [:run-right] (call-move action)
             [:run-left] (call-move action)
             [:stand] (call-move action)
             :else (do
                     (println "Unknown action")
                     (tuples/tuple 0 [state]))))))

(defn- reward-com [world actions rewards terminal?]
  (let [η (eta world actions)
        Ω (omega world actions)]
    (fn [state action]
      (let [[time path] (Ω state action)
            hits (->> path (map η) (flatten) (filter #(:solid? %)))
            holes (filter #(hole? %) path)
            end ((or-else (fn [_]
                            (:end rewards)) 0) (find-some #(terminal? %) path))]
        (+ (* time (:tic rewards))
           (* (count hits) (:solid rewards))
           (* (count holes) (:solid rewards))
           end)))))

(defn- transition-com [world actions terminal?]
  (let [Ω (omega world actions)]
    (fn [state action]
      (let [[_ path] (Ω state action)
            ;_ (print state)
            ;_ (print " ")
            ;_ (print action)
            ;_ (println path)
            ]
        (if-let [end (find-some terminal? path)]
          end
          (cond
            (some #(out? (:position %)) path)
            state
            :else (last path)))))))

(defn reward
  ([world terminal?]
   (reward world all-actions Rewards terminal?))
  ([world actions terminal?]
   (reward-com world actions Rewards terminal?))
  ([world actions rewards terminal?]
   (reward-com world actions rewards terminal?)))

(defn transition
  ([world terminal?]
   (transition world all-actions terminal?))
  ([world actions terminal?]
   (transition-com world actions terminal?)))

(defn terminal? [world]
  (let [max (max-by #(-> % (:position) (:x)) world)]
    (fn [state]
      (>= (-> state (:position) (:x)) (-> max (:position) (:x))))))

(defn actions [_]
  (-> all-actions (drop-last) (keys)))
