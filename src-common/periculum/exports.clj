(ns periculum.exports
  (:require [clj-tuple :as t]
            [clojure.core.async :as async]
            [semantic-csv.core :as s]
            [periculum.more :as m]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [periculum.rl :as rl]
            [clojure-csv.core :as c]
            [flatland.useful.seq :as f]))

(def ^:const exp-path "/home/robert/Documents/Thesis/experiments/")

(defrecord Experiment [label index traces])
(defrecord Trace [attempt label episode reward optimum? cpu])
(defrecord Result [attempt label converged? episode reward delta-cpu])

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

(def data-casts {:episode   s/->int
                 :mean      s/->long
                 :variance  s/->long
                 :deviation s/->long})

(def result-casts {:attempt    s/->int
                   :converged? ->boolean
                   :episode    s/->int
                   :reward     s/->double
                   :delta-cpu  s/->long})

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

(defn path-decode [follow optimal?]
  (fn [body]
    (let [reward (->> body :data follow rl/total-reward)]
      (trace (:episode body)
             reward
             (optimal? reward)
             (:cpu body)))))

(defn export! [experiment]
  (with-open [out (io/writer (str exp-path
                                  (:label experiment)
                                  (:index experiment)
                                  ".csv"))]
    (write! out trace-cast (:traces experiment))))

(defn result [upper-bound traces]
  (->> traces
       (partition-by :optimum?)
       (filter #(and (-> % first :optimum?)
                     (>= (count %) upper-bound)))
       (pick traces)
       (make-result traces)))

(defn import! [from]
  (with-open [in (io/reader (str exp-path from))]
    (->>
      (c/parse-csv in)
      (s/remove-comments)
      (s/mappify)
      (s/cast-with trace-cast)
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

;; FIXME are these sample standard deviations, because I sample at every n episodes?
;; If so, then add Bessel's correction for the second mean 1 / (N - 1)
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
  (with-open [out (io/writer (str exp-path (-> experiments first first :label) "_data.csv"))]
    (let [means (means experiments)
          variances (variances experiments)
          std-devs (std-deviations experiments)]
      (->> (f/zip means variances std-devs)
           (map (fn [[m v s]] {:label     (:label m)
                               :episode   (:episode m)
                               :mean      (:mean m)
                               :variance  (:deviation v)
                               :deviation (:deviation s)}))
           (write! out data-casts)))))

(defn results! [upper-bound experiments]
  (with-open [out (io/writer (str exp-path (-> experiments first first :label) "_results.csv"))]
    (->> experiments
         (map #(result upper-bound %))
         (write! out result-casts))))

(defn listen! [channel experiment decode]
  (async/go-loop [x experiment]
    (if-let [data (async/<! channel)]
      (->> data (decode) (preamble x) (add-trace x) (recur))
      (export! x))))

