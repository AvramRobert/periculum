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

(defn path-supplier [algorithm eps]
  (let [chan (async/chan 4096)
        data (conf 1.0 0.2 0.7)
        start (state 1 1 :stand)
        policy (eps-greedy 0.7)
        terminal-f (terminal? world)
        action-f actions
        transition-f (transition world terminal-f)
        reward-f (reward world terminal-f)
        exp-algorithm (algorithm policy action-f reward-f transition-f terminal-f)
        sampling (make-sample reward-f)
        plot-f (fn [data]
                 (let [e (async/<!! expect-channel)
                       f (mse-per-epsiode (sampling e))]
                   (f data)))
        capture-f (capture-greedy start transition-f reward-f terminal-f)
        run! (control-> exp-algorithm
                        data
                        chan
                        #(and (> % 100) (zero? (mod % 100))))
        _ (run! start eps)
        _ (monitor chan capture-f plot-f)
        ;best (derive-path transition-f reward-f terminal-f)
        ]))

;; FIXME: PLEASE, MACRO. WRITE A BLOODY MACRO. PLEASE

;; FIXME: Something is fucked up in my dsl

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

(defn plot
  ([modules]
   (plot (:data-set-f modules) modules))
  ([as-dataset modules]
   (let [{start        :start
          reward-f     :reward
          transition-f :transition
          terminal-f   :terminal} modules
         capture-f (capture-greedy start transition-f reward-f terminal-f)]
     (assoc modules
       :plot-channel (async/chan 4096)
       :data-capture capture-f
       :data-set-f as-dataset))))

(def plot-mean-squared
  (fn [modules]
    (let [exp-f (fn []
                  (let [e (async/<!! expect-channel)]
                    (repeat e)))
          {start        :start
           reward-f     :reward
           transition-f :transition
           terminal-f   :terminal} modules
          g (make-sample reward-f)
          capture-f (capture-greedy start transition-f reward-f terminal-f)]
      (assoc modules
        :plot-channel (async/chan 4096)
        :data-capture capture-f
        :data-set-f (fn []
                      (let [expect (exp-f)]
                        (fn [data]
                          (let [f (mse-per-epsiode (->> expect first g))]
                            (f data)))))))))


(defn echo-best [modules]
  (let [{start        :start
         transition-f :transition
         reward-f     :reward
         terminal-f   :terminal
         channel      :q-channel
         } modules
        best-path (derive-path transition-f reward-f terminal-f)]
    (let [new-data (async/<!! channel)
          path (best-path start new-data)]
      (for [sample path]
        (async/go (async/>! result-channel sample))))))

(defn do-run!
  ([eps modules]
   (let [{start     :start
          data      :data
          algorithm :algorithm} modules
         run! (control algorithm data)
         channel (run! start eps)]
     (assoc modules
       :q-channel channel)))
  ([eps p modules]
   (let [{start      :start
          data       :data
          algorithm  :algorithm
          channel    :plot-channel
          capture-f  :data-capture
          as-dataset :data-set-f} modules
         run! (control-> algorithm data channel p)
         res-channel (run! start eps)
         _ (-> channel (monitor capture-f
                                (as-dataset)))]
     (assoc modules
       :q-channel res-channel))))


(defn continually [f modules]
  (letfn [(r [modules]
            (fn [eps]
              (let [res (f eps modules)]
                (r res))))]
    (r modules)))

;; FIXME: Expand
;; FIXME: Look at `control->`, make it return its q-values and implement a `continue` function
;; `continue` should handle the boilerplate of continuing the computation

(defn example [eps]
  (->> world
       (primary)
       (policy (eps-greedy 0.6))
       (data 1.0)
       (start-with 1 1)
       (algorithm sarsa-max)
       (do-run! eps)                                        ;#(and (> % 200) (zero? (mod % 100)))
       (echo-best)))
