(ns periculum.rl
  (:require [clojure.core.async :as async]
            [clj-tuple :as tuples])
  (:use [periculum.coll.more])
  (:use [handy.map-values]))

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

;; tested
(defn Gt [chain γ]
  (consume (fn [sample rem]
             (let [discounted (reduce + (discount rem γ))]
               (assoc sample :reward discounted))) chain))

;; tested
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

(defn from-chain [data chain f]
  (reduce (fn [new-data sample]
            (update-in new-data [(:state sample) (:action sample)]
                       #(f % sample))) data chain))

;; Monte Carlo

(defn every-visit-inc [counts sample]
  (update-in counts [(:state sample) (:action sample)] inc))

(defn mc-update [Q-SA Gt-SA N-SA]
  (+ Q-SA (* (/ 1 N-SA) (- Gt-SA Q-SA))))

(defn mc-eval [data chain]
  (let [γ (:gamma data)
        R-n (Gt chain γ)
        updated-data (update data :counts #(every-visit-inc % chain))]
    (reduce
      (fn [new-data sample]
        (let [{S :state
               A :action
               R :reward} sample
              N-SA (C new-data (->Pair S A))]
          (update-in new-data [S A]
                     #(mc-update % R N-SA)))) updated-data R-n)))

(defn mc-eval [data chain]
  (let [γ (:gamma data)
        R-n (Gt chain γ)
        updated-data (update data :counts #(every-visit-inc % chain))]
    (from-chain updated-data R-n
                (fn [Q-SA sample]
                  (let [{S :state
                         A :action
                         R :reward} sample
                        N-SA (C updated-data S A)]
                    (mc-update Q-SA R N-SA))))))

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

(defn sarsa-n-eval [data chain]
  (let [γ (:gamma data)
        {S' :state
         A' :action} (last chain)
        qt (drop-last (Gt chain γ))
        γ-S'A' (Math/pow (count chain) γ)
        Q-S'A' (Q data S' A')
        qtn (conj qt (->Sample S' A' (* γ-S'A' Q-S'A')))
        updated-data (from-chain data qtn
                                 (fn [Q-SA sample]
                                   (sarsa-n-update Q-SA (:reward sample) (:alpha data))))]
    (tuples/tuple S' updated-data)))

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
             cur-data data]
        (let [trajectory (take N (gen S))
              [S' new-data] (sarsa-n-eval cur-data trajectory)]
          (if (is-end? S')
            new-data
            (recur S' new-data)))))))

;; SARSA(λ) -> backward view

(defn Q-sarsa-λ [data S As δ]
  (let [α (:alpha data)]
    (map-assoc
      (fn [A R]
        (let [E (C data S A)]
          (+ R (* α δ E)))) As)))

(defn E-sarsa-λ [data S]
  (let [α (:alpha data)
        λ (:lambda data)
        As (get-in data [:counts S])]
    (map-values
      #(* α λ %) As)))

(defn sarsa-λ-update [data δ]
  (reduce
    (fn [new-data [S As]]
      (let [Q-SA (Q-sarsa-λ new-data S As δ)
            E-SA (E-sarsa-λ new-data S)
            update1 (assoc-in new-data [:q-values S] Q-SA)
            update2 (assoc-in update1 [:counts S] E-SA)]
        update2)) data (:q-values data)))


(defn sarsa-λ-eval [policy
                    action-f
                    reward-f
                    transition-f]
  (fn [data S A]
    (let [γ (:gamma data)
          R (reward-f S A)
          S' (transition-f S A)
          A' (policy S' action-f)
          Q-S'A' (Q data S' A')
          Q-SA (Q data S A)
          δ (+ R (- (* γ Q-S'A') Q-SA))
          updated-data (update-in data [:counts S A] inc)]
      (tuples/tuple S' A' (sarsa-λ-update updated-data δ)))))

;; FIXME: Add GLIE schedule
;; FIXME: Consider varying step sizes for better convergence properties
(defn sarsa-λ [policy
               action-f
               reward-f
               transition-f
               is-end?]
  (fn [start data]
    (let [λ-eval (sarsa-λ-eval policy action-f reward-f transition-f)]
      (loop [S start
             A (policy S action-f)
             new-data data]
        (let [[S' A' updated-data] (λ-eval new-data S A)]
          (if (is-end? S')
            updated-data
            (recur S' A' updated-data)))))))