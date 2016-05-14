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
    (Thread/sleep 10)
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

(defn- close-all [& channels]
  (if (empty? channels)
    (println "Successfully closed all channels")
    (let [chan (first channels)
          _ (async/close! chan)]
      (recur (rest channels)))))

(defn control<- [algorithm config & channels]
  (fn [start eps]
    (async/thread
      (do
        (reduce (fn [data episode]
                  (algorithm start data episode)) config (range 1 eps))
        (apply close-all channels)))))

;; ========= Utils =========

(defn discount [chain γ]
  (map-indexed (fn [idx sample]
                 (* (:reward sample) (Math/pow γ idx))) chain))

(defn Gt [chain γ]
  (consume (fn [sample rem]
             (let [discounted (reduce + (discount rem γ))]
               (assoc sample :reward discounted))) chain))

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
  (reduce
    (fn [new-data sample]
      (update-in new-data [:q-values (:state sample) (:action sample)]
                 (fn [old]
                   ((or-else #(f % sample) (:reward sample)) old)))) data chain))
(defn every-visit-inc
  ([data chain]
   (reduce (fn [n-data sample]
             (update-in n-data [:counts (:state sample) (:action sample)]
                        #((or-else inc 1) %))) data chain))
  ([data S A]
   (update-in data [:counts S A] #((or-else inc 1) %))))

(defn greedy-by-min [As]
  (-> (min-by val As) (keys) (first)))

(defn greedy-by-max [As]
  (-> (max-by val As) (keys) (first)))

(defn- simple-eval [policy-eval eps-count]
  (fn [[S new-data]]
    (policy-eval S new-data eps-count)))

(defn- echo-eval [channel policy-eval eps-count]
  (fn [[S prev-data]]
    (let [[S' new-data] (policy-eval S prev-data eps-count)
          _ (async/go (async/>! channel new-data))]
      (tuples/tuple S' new-data))))

(defn- bootstrap-policy [channel policy]
  (fn [S As data eps-count]
    (let [A (policy S As data eps-count)
          _ (async/go (async/>! channel (->Pair S A)))]
      A)))

(defn- bootstrap-eval
  ([policy-eval terminal? f]
   (fn [start data eps-count]
     (let [[_ new-data] (apply-while (fn [[S _]] (not (terminal? S)))
                                     (simple-eval policy-eval eps-count)
                                     (tuples/tuple start data))]
       (f new-data))))
  ([channel policy-eval terminal? f]
   (fn [start data eps-count]
     (let [[_ new-data] (apply-while (fn [[S _]] (not (terminal? S)))
                                     (echo-eval channel policy-eval eps-count)
                                     (tuples/tuple start data))]
       (f new-data)))))


;; ========= Policies =========

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

(defn ε-greedy
  ([ε]
   (ε-greedy ε greedy-by-max))
  ([ε find-opt]
   (ε-policy ε find-opt (fn [ε-in _] ε-in)))
  ([ε find-greedily channel]
   (let [policy (ε-greedy ε find-greedily)]
     (bootstrap-policy channel policy))))

(defn GLIE-ε-greedy
  ([ε]
   (GLIE-ε-greedy ε greedy-by-max))
  ([ε find-opt]
   (ε-policy ε find-opt
             (fn [ε-in k]
               (let [count (if (zero? k) 1 k)]
                 (* ε-in (/ 1 count))))))
  ([ε find-opt channel]
   (let [policy (GLIE-ε-greedy ε find-opt)]
     (bootstrap-policy channel policy))))

(defn GLIE-ε-episode
  ([find-greedily]
   (ε-policy 0.0 find-greedily (fn [_ k]
                                 (/ 1 k))))
  ([channel
    find-greedily]
   (let [policy (GLIE-ε-episode find-greedily)]
     (bootstrap-policy channel policy))))

(defn ε-balanced
  ([S As data eps-count]
   (pick-rnd As))
  ([channel]
   (bootstrap-policy channel ε-balanced)))

(def eps-balanced ε-balanced)

(def eps-greedy ε-greedy)

(def GLIE-eps-greedy GLIE-ε-greedy)

(def GLIE-eps-epsiode GLIE-ε-episode)

(defn greedy
  ([find-greedily]
   (fn [S As data _]
     (if-let [known-As (get (:q-values data) S)]
       (find-greedily known-As)
       (pick-rnd As))))
  ([channel
    find-greedily]
   (let [policy (greedy find-greedily)]
     (bootstrap-policy channel policy))))

;; ============ Monte Carlo ============

;; FIXME: rewrite Monte Carlo analogous to the other algorithms
;; Make the evaluation create the chain

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

(defn monte-carlo<- [channel
                     policy
                     action-f
                     reward-f
                     transition-f
                     terminal?]
  (fn [start data eps-count]
    (let [gen (lazy-traj policy action-f reward-f transition-f)
          trajectory (take-while #(not (terminal? (:state %))) (gen start data eps-count))
          new-data (mc-eval data trajectory)
          _ (async/go (async/>! channel new-data))]
      new-data)))

;; ========= SARSA(1) =========

(defn sarsa-update [S A R S' A' data]
  (let [α (:alpha data)
        γ (:gamma data)
        Q-SA (Q data S A)
        Q-S'A' (Q data S' A')]
    (assoc-in data [:q-values S A] (+ Q-SA (* α (+ R (- (* γ Q-S'A') Q-SA)))))))

(defn sarsa-eval [policy
                  action-f
                  reward-f
                  transition-f]
  (fn [SA data eps-count]
    (let [{S :state
           A :action} SA
          R (reward-f S A)
          S' (transition-f S A)
          A' (policy S' (action-f S') data eps-count)]
      (tuples/tuple (->Pair S' A') (sarsa-update S A R S' A' data)))))

(defn sarsa [policy
             action-f
             reward-f
             transition-f
             terminal?]
  (let [sarsa-evl (sarsa-eval policy action-f reward-f transition-f)]
    (fn [S data eps-count]
      (let [A (policy S (action-f S) data eps-count)
            evaluator (bootstrap-eval sarsa-evl
                                      #(terminal? (:state %))
                                      identity)]
        (evaluator (->Pair S A) data eps-count)))))

(defn sarsa<- [channel
               policy
               action-f
               reward-f
               transition-f
               terminal?]
  (let [sarsa-evl (sarsa-eval policy action-f reward-f transition-f)]
    (bootstrap-eval channel
                    sarsa-evl
                    #(terminal? (:state %))
                    identity)))

;;  ========= SARSA(λ) =========

(defn reset-eligibilities [data]
  (assoc data :counts {}))

(defn td-error [S A R S' A' data]
  (let [γ (:gamma data)
        Q-S'A' (Q data S' A')
        Q-SA (Q data S A)]
    (+ R (- (* γ Q-S'A') Q-SA))))

(defn Q-sarsa-λ [data S As δ]
  (let [α (:alpha data)]
    (map-assoc
      (fn [A R]
        (let [E (C data S A)]
          (+ R (* α δ E)))) As)))

(defn E-sarsa-λ [data S]
  (let [γ (:gamma data)
        λ (:lambda data)
        As (get-in data [:counts S])]
    (map-assoc
      (fn [_ C]
        (* γ λ C)) As)))

(defn sarsa-λ-update [SA data δ]
  (let [{S :state
         A :action} SA
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
  (fn [SA data eps-count]
    (let [{S :state
           A :action} SA
          R (reward-f S A)
          S' (transition-f S A)
          A' (policy S' (action-f S') data eps-count)
          δ (td-error S A R S' A' data)
          updated-data (every-visit-inc data S A)]
      (tuples/tuple (->Pair S' A') (sarsa-λ-update (->Pair S A) updated-data δ)))))

(defn sarsa-λ [policy
               action-f
               reward-f
               transition-f
               terminal?]
  (let [λ-eval (sarsa-λ-eval policy action-f reward-f transition-f)]
    (fn [S data eps-count]
      (let [A (policy S (action-f S) data eps-count)
            evaluator (bootstrap-eval λ-eval
                                      #(terminal? (:state %))
                                      reset-eligibilities)]
        (evaluator (->Pair S A) data eps-count)))))

(defn sarsa-λ<- [channel
                 policy
                 action-f
                 reward-f
                 transition-f
                 terminal?]
  (let [λ-eval (sarsa-λ-eval policy action-f reward-f transition-f)]
    (fn [S data eps-count]
      (let [A (policy S (action-f S) data eps-count)
            evaluator (bootstrap-eval channel
                                      λ-eval
                                      #(terminal? (:state %))
                                      reset-eligibilities)]
        (evaluator (->Pair S A) data eps-count)))))

;; ========= Q-Learning =========

(defn q-learning-eval [policy
                       greedy-policy
                       action-f
                       reward-f
                       transition-f]
  (fn [S data eps-count]
    (let [A (policy S (action-f S) data eps-count)
          R (reward-f S A)
          S' (transition-f S A)
          A' (greedy-policy S' (action-f S') data eps-count)]
      (tuples/tuple S' (sarsa-update S A R S' A' data)))))


(defn q-learning [find-greedily
                  policy
                  action-f
                  reward-f
                  transition-f
                  terminal?]
  (let [greedy-policy (greedy find-greedily)
        q-eval (q-learning-eval policy greedy-policy action-f reward-f transition-f)]
    (bootstrap-eval q-eval
                    terminal?
                    identity)))

(defn q-learning<- [channel
                    find-greedily
                    policy
                    action-f
                    reward-f
                    transition-f
                    terminal?]
  (let [greedy-policy (greedy find-greedily)
        q-eval (q-learning-eval policy greedy-policy action-f reward-f transition-f)]
    (bootstrap-eval channel
                    q-eval
                    terminal?
                    identity)))