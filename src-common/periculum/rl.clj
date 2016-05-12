(ns periculum.rl
  (:require [clojure.core.async :as async]
            [clj-tuple :as tuples])
  (:use [periculum.more])
  (:use [handy.map-values]))

(def empty-data {:q-values {}
                 :counts   {}
                 :gamma    0.0
                 :alpha    0.0
                 :lambda   0.0
                 })

(defn conf [gamma alpha lambda]
  (assoc empty-data :gamma gamma
                    :alpha alpha
                    :lambda lambda))

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
  (fn [start eps]
    (reduce
      (fn [data episode]
        (let [new-data (algorithm start data episode)
              _ (f new-data)]
          new-data)) config (range 1 eps))))

(defn control-> [algorithm config channel]
  (fn [start eps]
    (async/thread
      (reduce
        (fn [data episode]
          (let [new-data (algorithm start data episode)
                _ (async/>!! channel new-data)]
            new-data))
        config (range 1 eps))
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
  (fn [start data eps-count]
    (let [S start
          A (policy S (action-f S) data eps-count)
          R (reward-f start A)]
      (iterate (fn [sample]
                 (let [S' (transition-f (:state sample)
                                        (:action sample))
                       A' (policy S' (action-f S') data eps-count)
                       R' (reward-f S' A')]
                   (->Sample S' A' R'))) (->Sample S A R)))))

;; tested
(defn Q
  ([data SA]
   ((or-else identity 0)
     (get-in data [:q-values (:state SA) (:action SA)])))
  ([data S A]
   ((or-else identity 0)
     (get-in data [:q-values S A]))))

;; tested
(defn C
  ([data SA]
   ((or-else identity 0)
     (get-in data [:counts (:state SA) (:action SA)])))
  ([data S A]
   ((or-else identity 0)
     (get-in data [:counts S A]))))

;; tested
(defn from-chain [data chain f]
  (reduce
    (fn [new-data sample]
      (update-in new-data [:q-values (:state sample) (:action sample)]
                 (fn [old]
                   ((or-else #(f % sample) (:reward sample)) old)))) data chain))
;; tested
(defn every-visit-inc
  ([data chain]
   (reduce (fn [n-data sample]
             (update-in n-data [:counts (:state sample) (:action sample)]
                        #((or-else inc 1) %))) data chain))
  ([data S A]
   (update-in data [:counts S A] #((or-else inc 1) %))))

;; Monte Carlo

;; tested
(defn mc-update [Q-SA Gt-SA N-SA]
  (+ Q-SA (* (/ 1 N-SA) (- Gt-SA Q-SA))))

(defn mc-eval [data chain]
  (let [R-n (Gt chain (:gamma data))
        updated-data (every-visit-inc data chain)]
    (from-chain updated-data R-n
                (fn [Q-SA sample]
                  (let [{S     :state
                         A     :action
                         Gt-SA :reward} sample
                        N-SA (C updated-data S A)]
                    (mc-update Q-SA Gt-SA N-SA))))))

(defn monte-carlo [policy
                   action-f
                   reward-f
                   transition-f
                   terminal?]
  (fn [start data eps-count]
    (let [gen (lazy-traj policy action-f reward-f transition-f)
          trajectory (take-while #(not (terminal? (:state %))) (gen start data eps-count))]
      (mc-eval data trajectory))))

;; SARSA(1)

(defn sarsa-update [S A R S' A' data]
  (let [alpha (:alpha data)
        gamma (:gamma data)
        Q-SA (Q data S A)
        Q-S'A' (Q data S' A')]
    (assoc-in data [:q-values S A] (+ Q-SA (* alpha (+ R (- (* gamma Q-S'A') Q-SA)))))))

(defn sarsa-eval [policy
                  action-f
                  transition-f
                  reward-f]
  (fn [S A data eps-count]
    (let [R (reward-f S A)
          S' (transition-f S A)
          A' (policy S' (action-f S') data eps-count)]
      (tuples/tuple S' A' (sarsa-update S A R S' A' data)))))

(defn sarsa [policy
             action-f
             transition-f
             reward-f
             terminal?]
  (fn [start data eps-count]
    (let [evaluate (sarsa-eval policy action-f transition-f reward-f)]
      (loop [S start
             A (policy S (action-f S) data eps-count)
             cur-data data]
        (let [[S' A' new-data] (evaluate S A cur-data eps-count)]
          (if (terminal? S')
            new-data
            (recur S' A' new-data)))))))

;; SARSA(λ) -> backward view

;; tested
(defn Q-sarsa-λ [data S As δ]
  (let [α (:alpha data)]
    (map-assoc
      (fn [A R]
        (let [E (C data S A)]
          (+ R (* α δ E)))) As)))

;; tested
(defn E-sarsa-λ [data S]
  (let [γ (:gamma data)
        λ (:lambda data)
        As (get-in data [:counts S])]
    (map-assoc
      (fn [_ C]
        (* γ λ C)) As)))

(defn sarsa-λ-update [sample data δ]
  (let [{S :state
         A :action} sample
        associated (update-in data [:q-values S A]
                              (fn [itm]
                                ((or-else identity 0) itm)))]
    (reduce
      (fn [new-data [S As]]
        (let [Q-SA (Q-sarsa-λ new-data S As δ)
              E-SA (E-sarsa-λ new-data S)
              update1 (assoc-in new-data [:q-values S] Q-SA)
              update2 (assoc-in update1 [:counts S] E-SA)]
          update2)) associated (:q-values associated))))

(defn sarsa-λ-eval [policy
                    action-f
                    reward-f
                    transition-f]
  (fn [S A data eps-count]
    (let [R (reward-f S A)
          S' (transition-f S A)
          A' (policy S' (action-f S') data eps-count)
          Q-S'A' (Q data S' A')
          Q-SA (Q data S A)
          δ (+ R (- (* (:gamma data) Q-S'A') Q-SA))
          updated-data (every-visit-inc data S A)]
      (tuples/tuple S' A' (sarsa-λ-update (->Sample S A R) updated-data δ)))))

(defn reset-eligibilities [data]
  (assoc data :counts {}))

;; FIXME: Consider varying step sizes for better convergence properties
;; FIXME: This is computationally a little too intensive
(defn sarsa-λ [policy
               action-f
               reward-f
               transition-f
               is-end?]
  (fn [start data eps-count]
    (let [λ-eval (sarsa-λ-eval policy action-f reward-f transition-f)]
      (loop [S start
             A (policy S (action-f S) data eps-count)
             cur-data data]
        (let [[S' A' new-data] (λ-eval S A cur-data eps-count)]
          (if (is-end? S')
            (reset-eligibilities new-data)
            (recur S' A' new-data)))))))


;; FIXME: Q-Learning


;; policies
(defn- ε-policy [ε find-opt f]
  (fn [S As data eps-count]
    (if (contains? (:q-values data) S)
      (let [Qs (:q-values data)
            nε (f ε eps-count)
            P-greedy (+ (/ nε (count As)) (- 1 ε))
            A-greedy (find-opt (get Qs S))
            rest (filter #(not (= A-greedy %)) As)]
        (if (> (rand) P-greedy)
          (pick-rnd rest)
          A-greedy))
      (pick-rnd As))))


(defn opt-by-min [As]
  (min-by val As))

(defn opt-by-max [As]
  (max-by val As))

(defn ε-greedy
  ([ε find-opt]
   (ε-policy ε find-opt (fn [ε-in _] ε-in)))
  ([ε]
   (ε-policy ε opt-by-max (fn [ε-in _] ε-in))))

(defn GLIE-ε-greedy [ε find-opt]
  (ε-policy ε find-opt
            (fn [ε-in k]
              (let [count (if (zero? k) 1 k)]
                (* ε-in (/ 1 count))))))

(defn GLIE-ε-episode [find-opt]
  (ε-policy 0.0 find-opt (fn [_ k]
                           (/ 1 k))))

(defn ε-balanced [S As data eps-count]
  (pick-rnd As))


(defn eps-balanced [S As data eps-count]
  (ε-balanced S As data eps-count))

(defn eps-greedy [epsilon find-opt]
  (ε-greedy epsilon find-opt))

(defn GLIE-eps-greedy [epsilon find-opt]
  (GLIE-ε-greedy epsilon find-opt))

(defn GLIE-eps-epsiode [find-opt]
  (GLIE-ε-episode find-opt))

