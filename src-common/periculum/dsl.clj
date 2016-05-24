(ns periculum.dsl
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.learning]
    [periculum.more]
    [periculum.plots])
  (:require
    [clojure.core.async :as async]
    [play-clj.core :refer [shape, color, color!]]))

(def ^:const local-path "/home/robert/Repositories/periculum/resources/")

(def policy-channel (async/chan 2048))
(def result-channel (async/chan))
(def plot-channel (async/chan 4096))

(defn state [x y action]
  (->State (->Pos x y) action))

(def world-config1 {:floor     (m-struct 16 0 (pos 0 0))
                    :holes     [(pos 3 0) (pos 4 0) (pos 9 0) (pos 10 0)]
                    :walls     [(m-struct 2 3 (pos 7 1))]
                    :platforms [(m-struct 2 (pos 10 5))]})

(def world-config2 {:floor     (m-struct 14 0 (pos 0 0))
                    :holes     [(pos 3 0) (pos 13 0) (pos 14 0) (pos 15 0)]
                    :walls     [(m-struct 2 3 (pos 6 1)) (m-struct 2 3 (pos 10 1))]
                    :platforms empty-vec
                    })

(def world (make-world world-config1))
;(def world (world-from-pixmap (str local-path "level1.png")))

(defn observe! [channel]
  (delayed-observer channel))

(def choice-observer (observe! policy-channel))

(defn plotted [channel algorithm start eps interval]
  (let [data (conf 1.0 0.4 0.7)
        policy (eps-greedy 0.6)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm policy action-f reward-f transition-f terminal-f)
        aggregate (monitor-greedily start action-f transition-f reward-f terminal-f)
        ctrl (control-> exp-algorithm data channel interval)
        _ (ctrl start eps)]
    (-> channel (aggregate exp-per-eps) as-lines)))

(defn path-supplier [algorithm eps]
  (let [data (conf 1.0 0.2 0.7)
        start (state 1 1 :stand)
        policy (eps-greedy 0.6)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm policy action-f reward-f transition-f terminal-f)
        ctrl (control exp-algorithm data)
        best (derive-path action-f transition-f reward-f terminal-f)]
    (let [thread-chan (ctrl start eps)
          new-data (async/<!! thread-chan)
          path (best start new-data)]
      path)))
;(for [e path]
;  (async/>!! result-channel e))

;; FIXME: PLEASE, MACRO. WRITE A BLOODY MACRO. PLEASE

(defn primary [loc-world]
  (let [terminal-f (terminal? loc-world)
        reward-f (reward loc-world terminal-f)
        transition-f (transition loc-world terminal-f)]
    {:terminal   terminal-f
     :action     actions
     :reward     reward-f
     :transition transition-f}))

(defn policy [f modules]
  (assoc modules :policy f))

(defn algorithm [f modules]
  (let [{terminal-f   :terminal
         action-f     :action
         reward-f     :reward
         transition-f :transition
         policy       :policy} modules]
    (assoc modules :algorithm (f policy action-f reward-f transition-f terminal-f))))

(defn data
  ([gamma modules]
   (data gamma 0.0 0.0 modules))
  ([gamma alpha modules]
   (data gamma alpha 0.0 modules))
  ([gamma alpha lambda modules]
   (assoc modules :data (conf gamma alpha lambda))))

(defn start-with
  ([x y modules]
   (start-with x y :stand modules))
  ([x y action modules]
   (assoc modules :start (state x y action))))

(defn plot-with [channel transform plot-f modules]
  (let [{start        :start
         action-f     :action
         reward-f     :reward
         transition-f :transition
         terminal-f   :terminal} modules
        aggregate (monitor-greedily start action-f transition-f reward-f terminal-f)]
    (assoc modules
      :plot-channel channel
      :aggregate aggregate
      :transformation transform
      :plotting-f plot-f)))

(defn do-run!
  ([eps modules]
   (let [{start     :start
          data      :data
          algorithm :algorithm} modules
         run (control algorithm data)]
     (run start eps)))
  ([eps interval modules]
   (let [{start     :start
          data      :data
          algorithm :algorithm
          channel   :plot-channel
          aggregate :aggregate
          transform :transformation
          plot-f    :plotting-f} modules
         run (control-> algorithm data channel interval)
         _ (run start eps)
         _ (-> channel (aggregate transform) plot-f)]
     )))

;; FIXME: Expand
;; FIXME: Look at `control->`, make it return its q-values and implement a `continue` and a `retry` function
;; `continue` should handle the boilerplate of continuing the computation
;; `retry` should handle the boilerplate of restarting the same computation

(defn example [eps]
  (->> world
       (primary)
       (policy (eps-greedy 0.6))
       (data 1.0)
       (start-with 1 1)
       (algorithm monte-carlo)
       (plot-with plot-channel exp-per-eps as-lines)
       (do-run! eps 100)))
