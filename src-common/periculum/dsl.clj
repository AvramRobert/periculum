(ns periculum.dsl
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.learning]
    [periculum.more]
    [periculum.plots])
  (:require
    [clojure.core.async :as async]
    [play-clj.core :refer [shape, color, color!]]
    [clj-tuple :as tuples]))

(def ^:const local-path "/home/robert/Repositories/periculum/resources/")

(def policy-channel (async/chan))
(def result-channel (async/chan))
(def expect-channel (async/chan))

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

(defn make-sample [reward-f]
  (fn [path]
    (map #(->Sample (:state %) (:action %) (reward-f (:state %) (:action %))) path)))

(defn- val-of [kvs k]
  (->> kvs (drop-while #(not= % k)) second))

(defn- exp-primitives [kvs]
  (let [w (val-of kvs :world)
        policy (val-of kvs :policy)
        t? (terminal? w)
        a actions
        r (reward w t?)
        t (transition w t?)]
    {:policy     policy
     :action     a
     :reward     r
     :transition t
     :terminal   t?}))

(defn- exp-algorithm [kvs]
  (let [prims (exp-primitives kvs)
        algo (val-of kvs :algorithm)]
    (algo (:policy prims)
          (:action prims)
          (:reward prims)
          (:transition prims)
          (:terminal prims))))

(defn- exp-data [kvs]
  (let [gamma (val-of kvs :gamma)
        alpha (val-of kvs :alpha)
        lambda (val-of kvs :lambda)]
    (apply conf (filter #(not (nil? %)) (tuples/tuple gamma alpha lambda)))))

(defn- exp-start [kvs]
  (-> kvs
      (val-of :start)
      ((or-else
         (fn [s]
           (state (first s) (second s) :stand))
         (state 1 1 :stand)))))

(defn- exp-plotf [prims kvs]
  (let [plot (val-of kvs :plot)
        title ((or-else identity "") (:title plot))
        greedily (capture-greedy (exp-start kvs)
                                 (:transition prims)
                                 (:reward prims)
                                 (:terminal prims))]
    (case (:method plot)
      :reward/episode [greedily (reward-per-episode title)]
      :reward/action [greedily (reward-per-action title)]
      :steps/episode [greedily (steps-per-episode title)]
      :mse/episode [greedily
                    (fn [data]
                      (let [with-rew (make-sample (:reward prims))
                            expectation (async/<!! expect-channel)
                            pf (mse-per-epsiode (with-rew expectation) title)]
                        (pf data)))]
      "Should not occur")))

(defn- exp-plot [kvs]
  (let [prims (exp-primitives kvs)
        [f plot] (exp-plotf prims kvs)]
    (fn [channel]
      (monitor channel f plot))))

(defn- plotted? [kvs]
  (some? (val-of kvs :plot)))

(defn- exp-schedule [kvs]
  (-> kvs
      (val-of :plot)
      (:schedule)))

(defn deflearn [& kvs]
  "Small DSL for working with the algorithm
   Parameters:
    :world => vector records
    :policy => function
    :gamma => number
    :alpha => number
    :lambda => number
    :algorithm => function
    :episodes => number
    :plot { :title => string
            :schedule => predicate
            :method => keyword
           }
  "
  (let [data (exp-data kvs)
        eps (val-of kvs :episodes)
        start (exp-start kvs)
        algorithm (exp-algorithm kvs)]
    (if (plotted? kvs)
      (let [schedule (exp-schedule kvs)
            pre-run! (fn [channel] (control-> algorithm data channel schedule))
            monitor! (exp-plot kvs)]
        (fn []
          (let [chan (async/chan 4096)
                res ((pre-run! chan) start eps)
                _ (monitor! chan)]
            res)))
      (let [run! (control-> algorithm data)]
        (fn []
          (run! start eps))))))