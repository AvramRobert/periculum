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

(defn- exp-start [kvs]
  (-> kvs
      (val-of :start)
      ((or-else
         identity
         (state 1 1 :stand)))))

(defn- with-mdp? [kvs]
  (every?
    (fn [action]
      (some #(= % action) kvs))
    [:start
     :terminal
     :action
     :reward
     :transition]))

(defn- primitives [kvs
                   action
                   reward
                   transition
                   terminal]
  (let [start (exp-start kvs)
        policy (val-of kvs :policy)]
    (assert world "Please specify a world")
    (assert policy "Please specify a policy")
    {:start      start
     :policy     policy
     :action     action
     :reward     reward
     :transition transition
     :terminal   terminal
     }))

(defn- exp-primitives [kvs]
  (if (with-mdp? kvs)
    (primitives kvs
                (val-of kvs :action)
                (val-of kvs :reward)
                (val-of kvs :transition)
                (val-of kvs :terminal))
    (let [w (val-of kvs :world)
          a actions
          t? (terminal? w)
          r (reward w t?)
          t (transition w t?)]
      (primitives kvs a r t t?))))

(defn- exp-algorithm [kvs]
  (let [prims (exp-primitives kvs)
        algo (val-of kvs :algorithm)]
    (assert algo "Please specify an algorithm")
    (algo (:policy prims)
          (:action prims)
          (:reward prims)
          (:transition prims)
          (:terminal prims))))

(defn from-data [kvs]
  (val-of kvs :data))

(defn- exp-data [kvs]
  (if-let [data (from-data kvs)]
    data
    (let [gamma (val-of kvs :gamma)
          alpha (val-of kvs :alpha)
          lambda (val-of kvs :lambda)]
      (assert gamma "Please specify a discount factor gamma")
      (apply conf (filter #(not (nil? %)) (tuples/tuple gamma alpha lambda))))))

(defn- deref-plot [plot prims]
  (let [title ((or-else identity "") (:title plot))
        greedily (capture-greedy (:start prims)
                                 (:transition prims)
                                 (:reward prims)
                                 (:terminal prims))]
    (assert plot "Please specify a plot method")
    (case (:method plot)
      :reward/episode [greedily (reward-per-episode title)]
      :reward/action [greedily (reward-per-action title)]
      :steps/episode [greedily (steps-per-episode title)]
      :value/state [identity (value-per-state title)]
      :mse/episode [greedily
                    (fn [data]
                      (let [with-rew (make-sample (:reward prims))
                            expectation (async/<!! expect-channel)
                            pf (mse-per-epsiode (with-rew expectation) title)]
                        (pf data)))]
      (println "Unknown method"))))

(defn- plotted? [kvs]
  (some? (val-of kvs :plots)))

(defn- echoed? [kvs]
  (val-of kvs :echo))

(defn- on-echo [kvs]
  (fn [channel]
    (if (echoed? kvs)
      (let [prims (exp-primitives kvs)
            start (:start prims)
            path (compute-path (:transition prims)
                               (:reward prims)
                               (:terminal prims))
            propagate (fn [p]
                        (foreach
                          #(async/go (async/>! result-channel %)) p)
                        channel)]
        (->> channel
             (async/<!!)
             (path start)
             propagate))
      channel)))

(defn- exp-plots [plots kvs]
  (let [prims (exp-primitives kvs)]
    (map
      (fn [plot]
        (let [[f plotf] (deref-plot plot prims)
              schedule (:schedule plot)
              monitor! (fn [channel] (monitor channel f plotf))]
          (tuples/hash-map
            :monitor monitor!
            :schedule schedule))) plots)))

(defn- with-plots [kvs]
  (-> kvs
      (val-of :plots)
      (tuples/tuple)
      (flatten)
      (exp-plots kvs)))

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
        echo (on-echo kvs)
        algorithm (exp-algorithm kvs)]
    (assert eps "Please specify a number of episodes")
    (if (plotted? kvs)
      (letfn [(apply-vk [[itm f]] (f itm))
              (run! [{schedules :schedules
                      monitors  :monitors}]
                (fn []
                  (let [channels (take (count schedules) (repeatedly async/chan))
                        ds (zipmap channels schedules)
                        ms (zipmap channels monitors)
                        f (control-> algorithm data (map apply-vk ds))
                        g (comp echo f)]
                    (do
                      (foreach apply-vk ms)
                      (g start eps)))))]
        (->> kvs
             (with-plots)
             (map #(update % :schedule (fn [s]
                                         (fn [channel]
                                           (tuples/tuple channel s)))))
             (reduce
               (fn [map {schedule :schedule
                         monitor  :monitor}]
                 (-> map
                     (update :schedules #(conj % schedule))
                     (update :monitors #(conj % monitor))))
               (tuples/hash-map
                 :schedules (tuples/tuple)
                 :monitors (tuples/tuple)))
             (run!)))
      (fn []
        (let [f (control-> algorithm data)
              run! (comp echo f)]
          (run! start eps))))))

(defn deflearn-cont [& kvs]
  "Lazy continous application of `deflearn`.
   After every invocation, it returns a function, which
   when called, feeds the previously generated q-values in the algorithm
   and runs it again, continuing the learning process.
   This then returns another function which does the same thing.
   "
  (let [attach (fn [data]
                 (if (from-data kvs)
                   (conj (drop 2 kvs) data :data)
                   (conj kvs data :data)))
        learning (apply deflearn kvs)
        recurse (fn [data]
                  (apply deflearn-cont (attach data)))]
    (fn []
      (let [channel (learning)
            qs (async/<!! channel)]
        (recurse qs)))))
