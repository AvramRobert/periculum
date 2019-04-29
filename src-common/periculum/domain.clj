(ns periculum.domain
  (:use [periculum.world]
        [periculum.more])
  (:require [clj-tuple :as tuples]
            [play-clj.math :as gmath]
            [clojure.core.match :refer [match]]))

(defrecord Action [velocity time orientation])
(defrecord State [position previous-action])

(defn gravity [h-max t-apex]
  (/ (* 2 h-max) (Math/pow t-apex 2)))

(defn jump-velocity [g-const h-max]
  (Math/sqrt (* 2 g-const h-max)))

(def H-max 4)
(def T-apex 2)
(def G (gravity H-max T-apex))

(def primitive-actions {:stand      (->Action 0 1 [0 0])
                        :walk-left  (->Action 1 1 [-1 0])
                        :walk-right (->Action 1 1 [1 0])
                        :run-left   (->Action 2 1 [-1 0])
                        :run-right  (->Action 2 1 [1 0])
                        :jump       (->Action (Math/round ^Float (jump-velocity G H-max)) T-apex [0 1])
                        :fall       (->Action G 1 [0 -1])})

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

(defn <+> [start actions]
  "Simple Linear interpolation"
  (let [t (/ 1 (interval actions))
        end (endpoint start actions)
        inter-f (interpolation start end)]
    (distinct (+++ t inter-f))))

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
          :else (let [non-solid (take-while+ not-solid? interpolated)]
                  (tuples/tuple (inc t) (into visited non-solid))))))))


(defn ascend [lookup]
  (fn [start other-acts]
    (let [T-apex (:time (lookup :jump))
          todo (deref-actions (conj other-acts :jump) lookup)]
      (reduce
        (fn [ps _]
          (let [path (<+> (last ps) todo)]
            (into ps (next path)))) (tuples/tuple start) (range 0 T-apex)))))

(defn fall
  "Simulates a fall"
  ([pos lookup actions]
   ((descend lookup) pos actions))
  ([pos lookup]
   ((descend lookup) pos empty-vec)))

(defn blocked? [pos lookup]
  (boolean (some :solid? (lookup pos))))

(defn move [lookup]
  "Simulates a movement"
  (fn [pos action]
    (let [todo (deref-actions (tuples/tuple action) lookup)
          interpolated (<+> pos todo)
          can-stand?   #(solid-beneath? % lookup)
          blocked?     #(blocked? % lookup)
          not-blocked?  (comp not blocked?)]
      (cond
        (and (every? can-stand? interpolated)
             (every? not-blocked? interpolated)) [1 interpolated]
        (and (every? can-stand? interpolated)
             (some blocked? interpolated)) [1 (take-while not-blocked? interpolated)]
        :else (let [solid (vec (take-while can-stand? interpolated))
                    non-solid (find-some #(not (can-stand? %)) interpolated)
                    [t descent] (fall non-solid lookup)]
                (tuples/tuple t (into solid descent)))))))

(defn jump [lookup]
  "Simulates a jump"
  (fn [pos other-action]
    (let [T-apex (:time (lookup :jump))
          other (tuples/tuple other-action)
          ascent ((ascend lookup) pos other)
          ascension (->> (fall (last ascent) lookup other)
                         (second)
                         (into ascent)
                         (take-while #(not (blocked? % lookup)))
                         (vec))
          path (if (solid-beneath? (last ascension) lookup) ascension
                   (->> (fall (last ascension) lookup) (second) (into ascension)))]
      [T-apex path])))


(defn to-state [time-position action]
  (let [[time ps] time-position]
    (tuples/tuple time (map #(->State % action) ps))))

(defn eta-gen [world actions f]
  (fn [item]
    (if (keyword? item)
      (item actions)
      (f item world))))

(defn eta-one [world actions]
  "Given a platformer world and the actions of this world, returns a closure.
  The closure will, given a state, the most relevant world entity, that is associated state"
  (eta-gen world
           actions
           (fn [item -world]
             (find-some #(= (:position item) (:position %)) -world))))

(defn eta [world actions]
  "Given a platformer world and the actions of this world, returns a closure.
  The closure will, given a state, return the world entities, that are associated with that state"
  (eta-gen world
           actions
           (fn [item -world]
             (filter #(= (:position item) (:position %)) -world))))


(defn eta-pos [world actions]
  "Given a platformer world and the actions of this world, returns a closure.
  The closure will, given a state, return the position of the most relevant entity, that is associated with that state"
  (eta-gen world
           actions
           (fn [item -world]
             (filter #(= (:position %) item) -world))))

(defn omega [world actions]
  "Given a platformer world and the available actions of this world, returns a closure.
  The closure will, given a state and an action, compute all states the agent implicitly vistits during a state transition
  and the time the transition took"
  (let [lookup (eta-pos world actions)]
    (fn [state action]
      (let [position (:position state)
            call-move #(to-state ((move lookup) position %) %)
            call-jump #(to-state ((jump lookup) position %) action)]
        (case action
          :jump-up (call-jump :stand)
          :jump-left (call-jump :walk-left)
          :jump-right (call-jump :walk-right)
          :run-jump-left (call-jump :run-left)
          :run-jump-right (call-jump :run-right)
          :walk-left (call-move action)
          :walk-right (call-move action)
          :run-left (call-move action)
          :run-right (call-move action)
          :stand (call-move action)
          (do
            (println "Unknown action: " action)
            (tuples/tuple 0 [state])))))))

(defn- reward-com [world actions terminal?]
  (let [η (eta world actions)
        Ω (omega world actions)]
    (fn [state action]
      (let [[_ path]  (Ω state action)
            hits      (->> path (map η) (flatten) (filter :solid?))
            holes     (filter hole? path)
            end?      (find-some terminal? path)
            unharmed? (= 0 (count hits) (count holes))]
        (if (and end? unharmed?) 1 -1)))))

(defn- transition-com [world actions terminal?]
  (let [Ω (omega world actions)]
    (fn [state action]
      (let [[_ path] (Ω state action)
            end (find-some terminal? path)]
        (cond
          (some? end) end
          (some #(out? (:position %)) path) state
          :else (last path))))))

(defn reward
  "Given a platformer world and the terminal function, it returns a closure.
  The closure will, given state and an action, give the appropriate reward for each state-action pair"
  ([world terminal?]
   (reward world primitive-actions terminal?))
  ([world actions terminal?]
   (reward-com world actions terminal?)))

(defn transition
  "Given a platformer world and a terminal function, it returns a closure.
  The closure will, given a state and an action, compute the next state of a state transition"
  ([world terminal?]
   (transition world primitive-actions terminal?))
  ([world actions terminal?]
   (transition-com world actions terminal?)))

(defn terminal?
  "Given a platformer world, it returns a closure. The closure will, given a state,
  appropriately detect end states"
  ([world] (terminal? world (fn [state max]
                              (>= (-> state :position :x)
                                  (-> max :position :x)))))
  ([world p]
   (let [max (max-by #(-> % :position :x) world)]
     (fn [state] (p state max)))))

(defn actions [S]
  "Given a state, it returns the available actions for that state"
  (case (:previous-action S)
    :run-left [:stand
               :walk-left
               :walk-right
               :run-left
               :run-right
               :run-jump-left]

    :run-right [:stand
                :walk-left
                :walk-right
                :run-left
                :run-right
                :run-jump-right]
    [:stand
     :walk-left
     :walk-right
     :jump-up
     :run-left
     :run-right
     :jump-left
     :jump-right]))