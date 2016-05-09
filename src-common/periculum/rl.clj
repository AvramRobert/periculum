(ns periculum.rl
  (:require [clojure.core.async :as async])
  (:use (periculum.coll.more))
  (:use (handy.map-values)))

(def empty-data {:q-values {}
                 :counts   {}
                 :gamma    0.0
                 :alpha    0.0
                 :lambda   0.0
                 })

(defrecord Pair [state action])
(defrecord Sample [state action reward])

(defn observer [channel]
  (async/go-loop [res {}]
    (println res)
    (if-let [data (async/<! channel)]
      (recur data)
      (do
        (println "Done")
        res))))

(defn control [algorithm config f]
  (fn [eps]
    (reduce
      (fn [data episode]
        (let [new-data (algorithm data episode)
              _ (f new-data)]
          new-data)) config (range 0 eps))))

(defn control-> [algorithm config channel]
  (fn [eps]
    (async/thread
      (reduce
        (fn [data episode]
          (let [new-data (algorithm data episode)
                _ (async/>!! channel new-data)]
            new-data))
        config (range 0 eps))
      (async/close! channel))
    ))

(defn discount [chain γ]
  (map-indexed (fn [idx sample]
                 (* (:reward sample) (Math/pow γ idx))) chain))

(defn Gt [chain γ]
  (consume (fn [elm rem]
             (assoc elm :reward (reduce + (discount rem γ)))) chain))

(defn lazy-traj [policy
                 action-f
                 reward-f
                 transition-f]
  (fn [start]
    (let [S start
          A (policy start action-f)
          R (reward-f start A)]
      (iterate (fn [sample]
                 (let [S' (transition-f (:state sample)
                                        (:action sample))
                       A' (policy S' action-f)
                       R' (reward-f S' A')]
                   (->Sample S' A' R'))) (->Sample S A R)))))

(defn Q
  ([data SA]
    ((or-else identity 0)
      (get-in data [:q-values (:state SA) (:action SA)])))
  ([data S A]
   ((or-else identity 0)
     (get-in data [:q-values S A]))))

(defn C
  ([data SA]
   ((or-else identity 0)
     (get-in data [:counts (:state SA) (:action SA)])))
  ([data S A]
   ((or-else identity 0)
     (get-in data [:counts S A]))))


;; Monte Carlo

(defn every-visit-inc [counts sample]
  (update-in counts [(:state sample) (:action sample)] inc))

(defn mc-update [Q-SA Gt-SA N-SA]
  (+ Q-SA (* (/ 1 N-SA) (- Gt-SA Q-SA))))

(defn mc-eval [data chain]
  (let [γ (:gamma data)
        discounted (Gt chain γ)
        updated-data (update data :counts #(every-visit-inc % chain))]
    (reduce
      (fn [new-data sample]
        (let [{S :state
               A :action
               R :reward} sample
              N-SA (C new-data (->Pair S A))]
          (update-in new-data [S A]
                     #(mc-update % R N-SA)))) updated-data discounted)))

;; FIXME: add GLIE control schedule
(defn monte-carlo [policy
                   action-f
                   reward-f
                   transition-f
                   is-end?]
  (fn [start data]
    (let [gen (lazy-traj policy action-f reward-f transition-f)
          trajectory (take-while #(not (is-end? %)) (gen start))]
      (mc-eval data trajectory))))

;; SARSA(n)

(defn sarsa-n-update [Q-SA Gt-SA α]
  (+ Q-SA (* α (- Gt-SA Q-SA))))

(defn sarsa-n-eval [data chain S'A']
  (let [γ (:gamma data)
        R-n-1 (Gt chain γ)
        γ-S'A' (Math/pow (count R-n-1) γ)
        Q-S'A' (Q data S'A')
        γ*Q-S'A' (* γ-S'A' Q-S'A')
        R-n (conj R-n-1 (->Sample (:state S'A') (:action S'A') γ*Q-S'A'))]
    (reduce
      (fn [new-data sample]
        (update-in new-data [(:state sample) (:action sample)]
                   #(sarsa-n-update % (:reward sample) (:alpha new-data)))) data R-n)))

;; FIXME: Add GLIE schedule
;; FIXME: Consider varying step sizes for better convergence properties
(defn sarsa-n [N
               policy
               action-f
               reward-f
               transition-f
               is-end?]
  (fn [start data]
    (let [gen (lazy-traj policy action-f reward-f transition-f)]
      (loop [S start
             A (policy S action-f)
             new-data data]
        (let [trajectory (take N (gen S))
              updated-data (sarsa-n-eval new-data trajectory (->Pair S A))
              S' (last trajectory)
              A' (policy S' action-f)]
          (if (is-end? S')
            updated-data
            (recur S' A' updated-data)))))))


;; SARSA(λ) -> backward view

(defn Q-sarsa-λ [data S As δ]
  (let [α (:alpha data)]
    (map-assoc
      (fn [A R]
        (let [E (C data (->Pair S A))]
          (+ R (* α δ E)))) As)))

(defn E-sarsa-λ [data S]
  (let [α (:alpha data)
        λ (:lambda data)
        As (get-in data [:counts S])]
    (map-values
      #(* α λ %) As)))

(defn update-sarsa-λ [data δ]
  (reduce
    (fn [new-data [S As]]
      (let [Q-SA (Q-sarsa-λ new-data S As δ)
            E-SA (E-sarsa-λ new-data S)
            update1 (assoc-in new-data [:q-values S] Q-SA)
            update2 (assoc-in update1 [:counts S] E-SA)]
        update2)) data (:q-values data)))

;; FIXME: Add GLIE schedule
;; FIXME: Consider varying step sizes for better convergence properties
(defn sarsa-λ [policy
               action-f
               reward-f
               transition-f
               is-end?]
  (fn [start data]
    (loop [S start
           A (policy S action-f)
           new-data data]
      (let [γ (:gamma new-data)
            R (reward-f S A)
            S' (transition-f S A)
            A' (policy S' action-f)
            Q-S'A' (Q new-data (->Pair S' A'))
            Q-SA (Q new-data (-> Pair S A))
            δ (+ R (- (* γ Q-S'A') Q-SA))
            updated-data (update-in new-data [:counts S A] inc)]
        (if (is-end? S')
          updated-data
          (recur S' A' (update-sarsa-λ updated-data δ)))))))