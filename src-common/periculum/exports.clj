(ns periculum.exports
  (:require [clj-tuple :as t]
            [clojure.core.async :as async]
            [semantic-csv.core :as s]
            [periculum.more :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [periculum.rl :as rl]))

(def ^:const exp-path "/home/robert/Documents/Thesis/experiments/")

(defrecord Experiment [label index traces results])
(defrecord Trace [label episode reward optimum? cpu])
(defrecord Result [label episode reward converged? dcpu])

(defn exp [label index]
  (->Experiment label index (t/tuple) (t/tuple)))

(defn trace [episode reward optimum? cpu]
  (->Trace "Trace" episode reward optimum? cpu))

(defn result
  ([exp] (->Result "Result"
                   (-> exp :traces last :episode)
                   (-> exp :traces last :reward)
                   false
                   (- (-> exp :traces last :cpu)
                      (-> exp :traces first :cpu))))
  ([eps rew conv? dcpu] (->Result "Result" eps rew conv? dcpu)))

(defn add-trace [experiment & ts]
  (update experiment :traces #(m/fuse % ts)))

(defn add-result [experiment & rs]
  (update experiment :results #(m/fuse % rs)))

(defn result? [experiment] (not (empty? (:results experiment))))

(defn finalise [experiment]
  (if (result? experiment)
    experiment
    (add-result experiment (result experiment))))

(defn casts [key]
  (case key
    :traces {:label    str
             :episode  int
             :reward   double
             :optimum? boolean
             :cpu      long}
    :results {:label      str
              :episode    int
              :reward     double
              :converged? boolean
              :dcpu       long}
    :none))


(defn- write! [out experiment key]
  (->> (get experiment key)
       (s/cast-with (casts key))
       (s/vectorize)
       (csv/write-csv out)))

(defn export! [experiment]
  (with-open [out (io/writer (str exp-path
                                  (:label experiment)
                                  (:index experiment)
                                  ".csv"))]
    (do
      (write! out experiment :traces)
      (write! out experiment :results))))

(defn path-decode [follow optimal?]
  (fn [body]
    (let [reward (->> body :data follow rl/total-reward)]
      (trace (:episode body)
             reward
             (optimal? reward)
             (:cpu body)))))

(defn check [max experiment]
  (let [relevant (->> experiment :traces reverse (take max) reverse vec)]
    (if (and
          (empty? (:results experiment))
          (->> relevant (map :optimum?) (reduce #(and %1 %2) true)))
      (add-result experiment (result (:episode (first relevant))
                                     (:reward (last relevant))
                                     true
                                     (- (-> experiment :traces last :cpu)
                                        (-> experiment :traces first :cpu))))
      experiment)))

(defn listen! [channel experiment decode]
  (async/go-loop [x experiment]
    (if-let [data (async/<! channel)]
      (->> data (decode) (add-trace x) (check 20) (recur))
      (->> x finalise export!))))

