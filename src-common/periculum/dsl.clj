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

(defn make-sample [reward-f]
  (fn [path]
    (map #(->Sample (:state %) (:action %) (reward-f (:state %) (:action %))) path)))

(defn val-of [kvs k]
  (->> kvs (drop-while #(not= % k)) second))

(defn- exp-start [kvs]
  (val-of kvs :start))

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
    (assert policy "Please specify a policy")
    (assert start "Please specify a starting state")
    {:start      start
     :policy     policy
     :action     action
     :reward     reward
     :transition transition
     :terminal   terminal
     }))

(defn- exp-primitives [kvs]
  (assert (with-mdp? kvs) "Please provide the necessary primitives for your MDP")
  (primitives kvs
              (val-of kvs :action)
              (val-of kvs :reward)
              (val-of kvs :transition)
              (val-of kvs :terminal)))

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
      :mse/episode (do
                     (assert (:expect plot) "Please specify an expectation channel")
                     [greedily
                      (fn [data]
                        (let [expect-channel (:expect plot)
                              with-rew (make-sample (:reward prims))
                              expectation (async/<!! expect-channel)
                              pf (mse-per-epsiode (with-rew expectation) title)]
                          (pf data)))])
      (println "Unknown method"))))

(defn- plotted? [kvs]
  (some? (val-of kvs :plots)))

(defn- echoed? [kvs]
  (val-of kvs :echo))

(defn- on-echo [kvs]
  (fn [channel]
    (if (echoed? kvs)
      (let [prims (exp-primitives kvs)
            echo-channel (val-of kvs :echo)
            start (:start prims)
            path (compute-path (:transition prims)
                               (:reward prims)
                               (:terminal prims))
            propagate (fn [p]
                        (async/go (async/>! echo-channel p))
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
  "Small DSL for working with the RL and plotting APIs
   Parameters:
    :world => vector records
    :policy => function
    :gamma => number
    :alpha => number
    :lambda => number
    :algorithm => function
    :episodes => number
    :start => your start state
    :action => function
    :reward => function
    :transition => function
    :terminal => predicate
    :plots [{:title => string
             :schedule => predicate
             :method => keyword
            }
           ...
            ]
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
