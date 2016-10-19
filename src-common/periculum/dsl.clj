(ns periculum.dsl
  (:use
    [periculum.world]
    [periculum.rl]
    [periculum.domain]
    [periculum.more]
    [periculum.plots])
  (:require
    [clojure.core.async :as async]
    [play-clj.core :refer [shape, color, color!]]
    [clj-tuple :as t]))


(defn- to-sample [reward-f]
  (fn [path]
    (map #(->Sample (:state %)
                    (:action %)
                    (reward-f (:state %) (:action %))) path)))

(defn extract [config key message]
  (let [item (get config key)]
    (assert item (str "Please specify: " message))
    item))

(defn start<- [config]
  (extract config :start "A starting state"))

(defn policy<- [config]
  (extract config :policy "A policy"))

(defn action<- [config]
  (extract config :action "An action function"))

(defn transition<- [config]
  (extract config :transition "A transition function"))

(defn reward<- [config]
  (extract config :reward "A reward function"))

(defn terminal<- [config]
  (extract config :terminal "A terminal function"))

(defn episodes<- [config]
  (extract config :episodes "A number of episodes"))

(defn echo<- [config]
  (extract config :echo "An echo channel"))

(defn plots<- [config]
  (extract config :plots "Plotting descriptions"))

(defn plot-type<- [plot]
  (extract plot :method "A plot type or method"))

(defn plot-schedule<- [plot]
  (extract plot :schedule "A plotting schedule"))

(defn plot-bridge<- [plot]
  (extract plot :expect "A bridge plotting channel for MSE"))

(defn gamma<- [config]
  (extract config :gamma "A discount factor gamma"))

(defn alpha<- [config]
  (extract config :alpha "A step-size alpha"))

(defn lambda<- [config]                                     ;; Not checked currently
  (:lambda config))

(defn population<- [config]                                 ;; Not checked currently
  (:population config))

(defn interval<- [config]                                   ;; Not checked currently
  (:interval config))

(defn generations<- [config]                                ;; Not checked currently
  (:generations config))

(defn elites<- [config]                                     ;; Not checked currently
  (:elites config))

(defn roulette<- [config]                                   ;; Not checked currently
  (:roulette config))

(defn tournament<- [config]
  (:tournament config))

(defn dispatches<- [config]                                 ;; Not checked currently
  (if-let [ds (:dispatches config)]
    (->> ds
         (t/tuple)
         (flatten)
         (map #(t/tuple (:channel %) (:schedule %))))
    (t/tuple)))

(defn- plotted? [config]
  (some? (:plots config)))

(defn- echoed? [config]
  (some? (:echo config)))

(defn- dispatched? [config]
  (some? (:dispatches config)))

(defn- parameters<- [config]
  (t/tuple
    (policy<- config)
    (action<- config)
    (reward<- config)
    (transition<- config)
    (terminal<- config)))

(defn- parameters-rnd<- [config]
  (t/tuple
    eps-balanced
    (action<- config)
    (reward<- config)
    (transition<- config)
    (terminal<- config)))

(defn primitives<- [config]
  {:start      (start<- config)
   :policy     (policy<- config)
   :action     (action<- config)
   :reward     (reward<- config)
   :transition (transition<- config)
   :terminal   (terminal<- config)})

(defn algorithm<- [config]
  (-> (extract config :algorithm "An appropriate algorithm")
      (apply (parameters<- config))))

(defn rollout<- [config]
  (apply rollout (parameters<- config)))

(defn resolver<- [config]
  (compute-path (transition<- config)
                (reward<- config)
                (terminal<- config)))

(defn data<- [config]
  (if-let [data (:data config)]
    data
    (econf
      (gamma<- config)
      (alpha<- config)
      (lambda<- config)
      (population<- config)
      (interval<- config)
      (generations<- config))))

(defn- deref-plot [plot prims]
  (let [title ((or-else identity "") (:title plot))
        plot-type (plot-type<- plot)
        greedily (capture-greedy (:start prims)
                                 (:transition prims)
                                 (:reward prims)
                                 (:terminal prims))]

    (case plot-type
      :reward/episode [greedily (reward-per-episode title)]
      :reward/action [greedily (reward-per-action title)]
      :steps/episode [greedily (steps-per-episode title)]
      :value/state [identity (value-per-state title)]
      :mse/episode (do
                     [greedily
                      (fn [data]
                        (let [expect-channel (plot-bridge<- plot)
                              with-rew (to-sample (:reward prims))
                              expectation (async/<!! expect-channel)
                              pf (mse-per-epsiode (with-rew expectation) title)]
                          (pf data)))])
      (println "Unknown method"))))

(defn- on-echo [config]
  (fn [channel]
    (if (echoed? config)
      (let [echo (echo<- config)
            start (start<- config)
            follow (resolver<- config)
            propagate #(async/go (async/>! echo %))
            qs (async/<!! channel)
            ret (async/chan)]
        (do
          (->> qs (follow start) (propagate))
          (async/put! ret qs)
          ret))
      channel)))

(defn- on-plot [config]
  (let [prims (primitives<- config)]
    (->> config
         (plots<-)
         (t/tuple)
         (flatten)
         (map (fn [plot]
                (let [[f plotf] (deref-plot plot prims)]
                  (t/hash-map
                    :monitor #(monitor % f plotf)
                    :schedule (plot-schedule<- plot))))))))

(defn deflearn [config]
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
    + :echo => channel (for explicit path display)
    :plots [{:title => string
             :schedule => predicate
             :method => keyword
             + :expect => channel (only when mse/episode is used)
            }
           ...
            ]
  "
  (let [data (data<- config)
        eps (episodes<- config)
        start (start<- config)
        echo (on-echo config)
        algorithm (algorithm<- config)]
    (if (plotted? config)
      (letfn [(k->v [[f itm]] (f itm))
              (run! [{schedules :schedules
                      monitors  :monitors}]
                (fn []
                  (let [channels (take (count schedules) (repeatedly async/chan))
                        ds (zipmap schedules channels)
                        ms (zipmap monitors channels)
                        f (control-> algorithm data (into (map k->v ds)
                                                          (dispatches<- config)))
                        g (comp echo f)]
                    (do
                      (foreach k->v ms)
                      (g start eps)))))]
        (->> config
             (on-plot)
             (map #(update % :schedule (fn [s]
                                         (fn [channel]
                                           (t/tuple channel s)))))
             (reduce
               (fn [map {schedule :schedule
                         monitor  :monitor}]
                 (-> map
                     (update :schedules #(conj % schedule))
                     (update :monitors #(conj % monitor))))
               (t/hash-map
                 :schedules (t/tuple)
                 :monitors (t/tuple)))
             (run!)))
      (fn []
        (let [f (control-> algorithm data (dispatches<- config))
              run! (comp echo f)]
          (run! start eps))))))

(defn deflearn-cont [config]
  "Lazy continous application of `deflearn`.
   After every invocation, it returns a function, which
   when called, feeds the previously generated q-values in the algorithm
   and runs it again, continuing the learning process.
   This then returns another function which does the same thing.
   "
  (let [learn (deflearn config)
        recurse #(deflearn-cont (assoc config :data %))]
    (fn []
      (->> (learn) (async/<!!) (recurse)))))
