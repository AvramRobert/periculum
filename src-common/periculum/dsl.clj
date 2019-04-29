(ns periculum.dsl
  (:require
    [clojure.core.async :as async]
    [periculum.rl :as rl]))

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

(defn gamma<- [config]
  (extract config :gamma "A discount factor gamma"))

(defn alpha<- [config]
  (extract config :alpha "A step-size alpha"))

(defn lambda<- [config]                                     ;; Not checked currently
  (:lambda config))

(defn dispatches<- [config]                                 ;; Not checked currently
  (letfn [(tuplify [m] [(:channel m) (:schedule m)])]
    (->> (get config :dispatches [])
         (mapv tuplify))))

(defn- echoed? [config]
  (some? (:echo config)))

(defn- parameters<- [config]
  [(policy<- config)
   (action<- config)
   (reward<- config)
   (transition<- config)
   (terminal<- config)])

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

(defn resolver<- [config]
  (rl/compute-path (transition<- config)
                   (reward<- config)
                   (terminal<- config)))

(defn data<- [config]
  (if-let [data (:data config)]
    data
    (rl/conf
      (gamma<- config)
      (alpha<- config)
      (lambda<- config))))

(defn- on-echo [config]
  (fn [channel]
    (if (echoed? config)
      (let [echo        (echo<- config)
            start       (start<- config)
            derive-path (resolver<- config)
            qs          (async/<!! channel)
            ret         (async/chan)
            propagate! #(async/go (async/>! echo %))
            _           (->> qs (derive-path start) (propagate!))
            _           (async/put! ret qs)]
        ret)
      channel)))

(defn deflearn [config]
  "Small DSL for working with the RL and plotting APIs
   Expects a map of the following parameters:
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
    + :dispatches => vector of the map: {:channel  core.async/channel
                                         :schedule (Int -> Bool)
                     (`schedule` is a predicate applied on the current episode
                      `channel` is a core.async channel that receives the data (see periculum.rl/empty-data) of the current learning process
                                every time the `schedule` predicate yields true)
    + :echo => core.async channel (pushes the q-values to the paltformer visualiser)"
  (let [data       (data<- config)
        eps        (episodes<- config)
        start      (start<- config)
        echo       (on-echo config)
        algorithm  (algorithm<- config)
        dispatches (dispatches<- config)
        _          (primitives<- config)]
    (fn []
      (let [f    (rl/control-> algorithm data dispatches)
            run! (comp echo f)]
        (run! start eps)))))