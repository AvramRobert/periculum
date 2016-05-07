(ns periculum.learning
  (:use [periculum.world])
  (:use [periculum.coll.more])
  (:require [clj-tuple :as tuples])
  (:require [clojure.core.match :refer [match]]))

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
(def R-tic -1)
(def R-solid -5)
(def R-end 20)

(def all-actions {:stand      (->Action 0 1 [0 0])
                  :walk-left  (->Action 1 1 [-1 0])
                  :walk-right (->Action 1 1 [1 0])
                  :run-left   (->Action 2 1 [-1 0])
                  :run-right  (->Action 2 1 [1 0])
                  :jump       (->Action (jump-velocity G H-max) T-apex [0 1])
                  :fall       (->Action G 1 [0 -1])
                  })

(defn update-pos [pos amount orient]
  (let [xed (assoc pos :x (+ (:x pos) (* amount (nth orient 0))))
        yed (assoc xed :y (+ (:y pos) (* amount (nth orient 1))))]
    yed))

(defn out? [pos]
  (or (neg? (:x pos)) (neg? (:y pos))))

(defn hole? [state]
  (zero? (:y (:position state))))

(defn solid? [cur-pos lookup]
  (let [item (some #(:solid? %) (lookup cur-pos))]
    (or (some? item) (out? cur-pos))))

(defn empty-beneath? [cur-pos lookup]
  (let [beneath (pos (:x cur-pos) (dec (:y cur-pos)))]
    (or (solid? beneath lookup) (out? cur-pos))))

(defn interval [key-actions]
  (let [maximum (max-by #(:velocity %) (map #(% all-actions) key-actions))]
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

(defn endpoint [pos acts]
  (reduce (fn [npos action]
            (let [{velocity    :velocity
                   time        :time
                   orientation :orientation} action
                  step (Math/round ^float (/ velocity time))]
              (update-pos npos step orientation))) pos (map #(% all-actions) acts)))

(defn <+> [start actions]
  (let [t (/ 1 (interval actions))
        end (endpoint start actions)
        inter-f (interpolation start end)]
    (distinct (+++ t inter-f))))

(defn descend [lookup]
  (fn [pos actions]
    (loop [t 0
           cur pos
           visited (tuples/tuple)]
      (let [interpolated (<+> cur (conj actions :fall))
            not-solid? #(not (solid? % lookup))]
        (if (every? not-solid? interpolated)
          (recur (inc t) (last interpolated) (into visited (drop-last interpolated)))
          (let [non-solid (take-while not-solid? interpolated)]
            (tuples/tuple (inc t) (into visited non-solid))))))))

(defn ascend [start other-acts]
  (let [with-jump (conj other-acts :jump)]
    (reduce
      (fn [ps _]
        (let [path (<+> (last ps) with-jump)]
          (into ps (next path)))) (tuples/tuple start) (range 0 T-apex))))

(defn fall
  ([pos lookup actions]
   ((descend lookup) pos actions))
  ([pos lookup]
   ((descend lookup) pos empty-vec)))

(defn move [lookup]
  (fn [pos action]
    (let [interpolated (<+> pos [action])
          can-stand? #(empty-beneath? % lookup)]
      (if (every? can-stand? interpolated)
        (tuples/tuple 1 interpolated)
        (let [solid (vec (take-while can-stand? interpolated))
              non-solid (find-some #(not (can-stand? %)) interpolated)
              [t fallen] (fall non-solid lookup)]
          (tuples/tuple t (into solid fallen)))))))

(defn jump [lookup]
  (fn [pos other-action]
    (let [other (tuples/tuple other-action)
          ascent (ascend pos other)
          [t descent] (fall (last ascent) lookup other)]
      (tuples/tuple (+ t T-apex) (into ascent descent)))))


(defn stateify [time-position action]
  (let [[time ps] time-position]
    (tuples/tuple time (map #(->State % action) ps))
    ))

(defn eta-one [world]
  (fn [state]
    (find-some #(= (:position state) (:position %)) world)))

(defn eta [world]
  (fn [state]
    (filter #(= (:position state) (:position %)) world)))

(defn eta-pos [world]
  (fn [position]
    (filter #(= (:position %) position) world)))

(defn omega [world]
  (fn [state action]
    (let [previous (:previous-action state)
          position (:position state)
          lookup (eta-pos world)]
      (match [action]
             [:jump] (stateify ((jump lookup) position previous) previous)
             :else (stateify ((move lookup) position action) action)))))

(defn reward [world is-end?]
  (let [η (eta world)
        Ω (omega world)]
    (fn [state action]
      (let [[time path] (Ω state action)
            hits (->> path (map η) (flatten) (filter #(:solid? %)))
            holes (filter #(hole? %) path)
            end ((or-else (fn [_]
                            R-end) 0) (find-some #(is-end? %) path))]
        (+ (* time R-tic)
           (* R-solid (count hits))
           (* R-solid (count holes))
           end)))))

; FIXME: Should the transition function teleport the agent back to his starting position if he falls in a hole?
(defn transition [world is-end?]
  (let [Ω (omega world)]
    (fn [state action]
      (let [[_ path] (Ω state action)]
        (if-let [end (find-some is-end? path)]
          end
          (last path))))))

(def actions (-> all-actions (drop-last) (keys)))
