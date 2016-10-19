(ns periculum.exports
  (:require [clj-tuple :as t]
            [clojure.core.async :as async]
            [semantic-csv.core :as s]
            [periculum.more :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [periculum.rl :as rl]
            [clojure-csv.core :as c]
            [incanter.stats :as stats]
            [flatland.useful.seq :as f]))

(def ^:const exp-path "/home/robert/Documents/Thesis/draft/experiments/")

(defrecord Experiment [label index traces])
(defrecord Trace [attempt label episode reward optimum? cpu])
(defrecord Result [attempt label converged? episode reward delta-cpu])
(defrecord Stat [experiments best worst converged failures mean-eps mean-cpu])

(defn exp [label index]
  (->Experiment label index (t/tuple)))

(defn trace [episode reward optimum? cpu]
  (->Trace 0 "" episode reward optimum? cpu))

(defn- add-trace [experiment & ts]
  (update experiment :traces #(m/fuse % ts)))

(defn ->boolean [s]
  (if (string? s)
    (-> s clojure.string/trim Boolean/parseBoolean)
    (boolean s)))

(def trace-cast {:attempt  s/->int
                 :episode  s/->int
                 :reward   s/->double
                 :optimum? ->boolean
                 :cpu      s/->long})

(def data-casts {:episode          s/->int
                 :mean-reward      s/->float
                 :variance-reward  s/->long
                 :deviation-reward s/->long})

(def result-casts {:attempt    s/->int
                   :converged? ->boolean
                   :episode    s/->int
                   :reward     s/->double
                   :delta-cpu  s/->long})

(def stats-casts {:experiments s/->int
                  :best        s/->int
                  :worst       s/->int
                  :converged   s/->int
                  :failures    s/->int
                  :mean-eps    s/->float
                  :mean-cpu    s/->long})

(defn- sqrd-diff [ts ms]
  (map (fn [trace]
         (map
           (fn [[t m]]
             {:label     (:label t)
              :episode   (:episode t)
              :deviation (Math/pow (- (:reward t) (:mean m)) 2)}) (f/zip trace ms))) ts))

(defn- re-sum [sqms]
  (reduce #(map
            (fn [[lt rt]]
              (update lt :deviation (fn [x] (+ x (:deviation rt))))) (f/zip %1 %2)) sqms))

(defn- map-sum [summed f] (map #(update % :deviation f) summed))

(defn- write! [out casts data]
  (->> data
       (s/cast-with casts)
       (s/vectorize)
       (csv/write-csv out)))

(defn- preamble [x pre-trace]
  (-> pre-trace
      (assoc :attempt (:index x))
      (assoc :label (:label x))))

(defn- pick [traces canidates]
  (if (empty? canidates)
    (last traces)
    (-> canidates first first)))

(defn- make-result [traces proper]
  (->Result
    (:attempt proper)
    (:label proper)
    (:optimum? proper)
    (:episode proper)
    (:reward proper)
    (- (:cpu proper)
       (:cpu (first traces)))))

(defn make-stats [results]
  (let [converged (filter :converged? results)]
    (->Stat
      (count results)
      (->> results (m/min-by :episode) :episode)
      (->> results (m/max-by :episode) :episode)
      (count converged)
      (- (count results) (count converged))
      (->> converged (map :episode) stats/mean float Math/round)
      (->> converged (map :delta-cpu) stats/mean))))

(defn- label [experiments]
  (-> experiments first first :label))

(defn path-decode [follow optimal?]
  (fn [body]
    (let [reward (->> body :data follow rl/total-reward)]
      (trace (:episode body)
             reward
             (optimal? reward)
             (:cpu body)))))

(defn export! [experiment]
  (with-open [out (io/writer (str exp-path
                                  (:index experiment)
                                  (:label experiment)
                                  ".csv"))]
    (write! out trace-cast (:traces experiment))))

(defn result [upper-bound traces]
  (->> traces
       (partition-by :optimum?)
       (filter #(and (-> % first :optimum?)
                     (>= (count %) upper-bound)))
       (pick traces)
       (make-result traces)))

(defn import-with! [casts from]
  (with-open [in (io/reader (str exp-path from))]
    (->>
      (c/parse-csv in)
      (s/remove-comments)
      (s/mappify)
      (s/cast-with casts)
      doall)))

(defn means [experiments]
  (->> experiments
       (reduce (fn [l r]
                 (map (fn [[tl tr]]
                        (update tl :reward #(+ % (:reward tr)))) (f/zip l r))))
       (map #(t/hash-map
              :label (:label %)
              :episode (:episode %)
              :mean (/ (:reward %) (count experiments))))))

(defn variances [experiments]
  (let [means (means experiments)]
    (-> experiments
        (sqrd-diff means)
        (re-sum)
        (map-sum #(/ % (count experiments))))))

(defn std-deviations [experiments]
  (let [means (means experiments)]
    (-> experiments
        (sqrd-diff means)
        (re-sum)
        (map-sum #(Math/sqrt (/ % (count experiments)))))))

(defn data! [experiments]
  (with-open [out (io/writer (str exp-path (label experiments) "_data.csv"))]
    (let [means (means experiments)
          variances (variances experiments)
          std-devs (std-deviations experiments)]
      (->> (f/zip means variances std-devs)
           (map (fn [[m v s]] {:episode          (:episode m)
                               :mean-reward      (:mean m)
                               :variance-reward  (:deviation v)
                               :deviation-reward (:deviation s)}))
           (write! out data-casts)))))

(defn import-exp!
  ([amount name]
   (import-exp! amount "" name))
  ([amount folder name]
   (for [n (range 0 amount)]
     (import-with! trace-cast (str folder n name ".csv")))))

(defn results! [upper-bound experiments]
  (with-open [out (io/writer (str exp-path (label experiments) "_results.csv"))]
    (->> experiments
         (map #(result upper-bound %))
         (write! out result-casts))))

(defn stats! [upper-bound experiments]
  (with-open [out (io/writer (str exp-path (label experiments) "_stats.csv"))]
    (->> experiments
         (map #(result upper-bound %))
         (make-stats)
         (t/tuple)
         (write! out stats-casts))))

(defn upper-bound [P experiments]
  "Let
    N(x) = number of traces in experiments
       E = number of chosen episodes
   => Ix = E / N(x), interval at which a trace was logged

   It is always desired to check the same amount of data for convergence, regardless of
   Ix, E and thus N(x). For any E and Ix, P percent of data should always be checked for convergence.

   => given P = percentage to be checked
            U = P * N(x), where U is the upper-bound for `results!`"
  (* P (count experiments)))

(defn listen! [channel experiment decode]
  (async/go-loop [x experiment]
    (if-let [data (async/<! channel)]
      (->> data (decode) (preamble x) (add-trace x) (recur))
      (export! x))))

