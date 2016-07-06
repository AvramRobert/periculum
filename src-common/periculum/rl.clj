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

(defrecord Pair [state action])
(defrecord Sample [state action reward])

(defn conf
  "Configuration format used for the algorithms"
  ([gamma]
   (conf gamma 0.0))
  ([gamma alpha]
   (conf gamma alpha 0.0))
  ([gamma alpha lambda]
   (assoc empty-data :gamma gamma
                     :alpha alpha
                     :lambda lambda)))

(defn- close-all! [channels]
  (if (empty? channels)
    (println "Successfully closed all channels")
    (let [chan (first channels)
          _ (async/close! chan)]
      (recur (rest channels)))))

(defn- dispatch! [data episode dispatches]
  (foreach
    (fn [[channel p]]
      (when (p episode)
        (async/go
          (async/>! channel {:episode episode
                             :data    data}))))
    dispatches))

(defn control [algorithm config]
  "Given a RL-Algorithm closure and some starting configuration, it returns a closure.
  The closure will, given a starting state and a number of episodes, run the algorithm
  for that amount of episodes in a separate thread"
  (fn [start eps]
    (async/thread
      (println "Executing")
      (reduce
        (fn [data episode]
          (when (zero? (mod episode 100))
            (println episode))
          (algorithm start data episode)) config (range 1 eps)))))

(defn control->
  "Given a RL-Algorithm closure, some starting configuration and a number of channels, it returns a closure.
  The closure will, given a starting state and a number of episodes, run the algorithm
  for that amount of episodes in a separate thread and echo the intermediate Q-Values in the provided channels."
  ([algorithm config]
   (control algorithm config))
  ([algorithm config dispatches]
   (fn [start eps]
     (async/thread
       (letfn [(destroy-return! [result-chan]
                 (close-all! (map first dispatches))
                 result-chan)]
         (->>
           (range 1 eps)
           (reduce
             (fn [data episode]
               (when (zero? (mod episode 100)) (println episode))
               (dispatch! data episode dispatches)
               (algorithm start data episode)) config)
           (destroy-return!)))))))

;; ========= Utils =========
(defn action-mean [As]
  (let [sum (reduce (fn [p [_ v]] (+ p v)) 0 As)]
    (/ sum (count As))))

(defn total-reward [chain]
  (reduce #(+ %1 (:reward %2)) 0.0 chain))

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
          R (reward-f S A)]
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

(defn every-visit-inc
  ([data chain]
   (reduce (fn [n-data sample]
             (update-in n-data [:counts (:state sample) (:action sample)]
                        #((or-else inc 1) %))) data chain))
  ([data S A]
   (update-in data [:counts S A] #((or-else inc 1) %))))

(defn greedy-by-min [As]
  "Given an action-reward representation `{:action1 :reward :action2 :reward ..}`,
  it chooses the action with the smallest expected reward"
  (-> (min-by val As) (keys) (first)))

(defn greedy-by-max [As]
  "Given an action-reward representation `{:action1 :reward :action2 :reward ..}`,
   it chooses the action with the largest expected reward"
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
  "Generic function for ε-based policies.
  Accepts a probability ε, a function for finding the greedy action and a mapping function `f`, which
  allows an additional transformation of the probability ε. `f` is a binary function with parameters ε and the
  current episode count"
  (fn [S As data eps-count]
    (if (contains? (:q-values data) S)
      (let [Qs (:q-values data)
            nε (f ε eps-count)
            P-greedy (+ (/ nε (count As)) (- 1 ε))
            A-greedy (find-opt (get Qs S))
            rest (filter #(not (= A-greedy %)) As)]
        (if (> (rand) P-greedy)
          (rand-nth rest)
          A-greedy))
      (rand-nth As))))

(defn ε-greedy
  "Given a probability ε, between [0, 1], it chooses with probability ε
  the greedy action from the Q-Values and with 1 - ε an exploratory action.
  It can accept an additional function, which states what the greedy action should be.
  It can accept an additional channel, where it writes the current state and chosen action"
  ([ε]
   (ε-greedy ε greedy-by-max))
  ([ε find-greedily]
   (ε-policy ε find-greedily (fn [ε-in _] ε-in)))
  ([ε find-greedily channel]
   (let [policy (ε-greedy ε find-greedily)]
     (bootstrap-policy channel policy))))

(defn GLIE-ε-greedy
  "Similar behaviour to `ε-greedy`, however it reduces ε after each episode with 1/k, where k is the current episode count"
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
  "Similar behaviour to `ε-greedy`, however ε is always preset to 1/k, where k is the current episode count"
  ([find-greedily]
   (ε-policy 0.0 find-greedily (fn [_ k]
                                 (/ 1 k))))
  ([channel
    find-greedily]
   (let [policy (GLIE-ε-episode find-greedily)]
     (bootstrap-policy channel policy))))

(defn ε-balanced
  "Chooses exploratory actions randomly"
  ([S As data eps-count]
   (rand-nth As))
  ([channel]
   (bootstrap-policy channel ε-balanced)))

;; ========= Aliases =========

(def eps-balanced ε-balanced)

(def eps-greedy ε-greedy)

(def GLIE-eps-greedy GLIE-ε-greedy)

(def GLIE-eps-epsiode GLIE-ε-episode)

(defn greedy
  ([find-greedily]
   (fn [S As data _]
     (if-let [known-As (get-in data [:q-values S])]
       (find-greedily known-As)
       (rand-nth As))))
  ([channel
    find-greedily]
   (let [policy (greedy find-greedily)]
     (bootstrap-policy channel policy))))

;; ============ Monte Carlo ============

(defn monte-carlo-update [data sample]
  (let [{S     :state
         A     :action
         Gt-SA :reward} sample
        Q-SA (Q data S A)
        N-SA (C data S A)]
    (assoc-in data [:q-values S A] (+ Q-SA (* (/ 1 N-SA) (- Gt-SA Q-SA))))))

(defn monte-carlo-eval [policy
                        action-f
                        reward-f
                        transition-f
                        terminal?]
  (fn [start data eps-count]
    (let [chain-from (lazy-traj policy action-f reward-f transition-f)
          markov-chain (take-while+ #(not (terminal? (:state %))) (chain-from start data eps-count))
          Gt-R (Gt markov-chain (:gamma data))
          data-counted (every-visit-inc data markov-chain)]
      (tuples/tuple start (reduce monte-carlo-update data-counted Gt-R)))))

(defn monte-carlo [policy
                   action-f
                   reward-f
                   transition-f
                   terminal?]
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
 it returns a closure. The closure will, given a start state, start data and current episode count,
 applies Monte Carlo and returns the learned Q-Values"
  (let [mc-eval (monte-carlo-eval policy action-f reward-f transition-f terminal?)]
    (bootstrap-eval mc-eval
                    (fn [_] true)
                    identity)))

(defn monte-carlo<- [channel
                     policy
                     action-f
                     reward-f
                     transition-f
                     terminal?]
  "Analogous to `monte-carlo`. It accepts an additional channel, where it writes its intermediate Q-Values"
  (let [mc-eval (monte-carlo-eval policy action-f reward-f transition-f terminal?)]
    (bootstrap-eval channel
                    mc-eval
                    (fn [_] true)
                    identity)))

;; ========= SARSA(1) =========

(defn sarsa-update [S A R S' A' data]
  (let [alpha (:alpha data)
        gamma (:gamma data)
        Q-SA (Q data S A)
        Q-S'A' (Q data S' A')]
    (assoc-in data [:q-values S A] (+ Q-SA (* alpha (+ R (- (* gamma Q-S'A') Q-SA)))))))

(defn sarsa-1-eval [policy
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

(defn sarsa-1 [policy
               action-f
               reward-f
               transition-f
               terminal?]
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
  it returns a closure. The closure will, given a start state, start data and current episode count,
  applies SARSA-1 and returns the learned Q-Values"
  (let [sarsa-eval (sarsa-1-eval policy action-f reward-f transition-f)]
    (fn [S data eps-count]
      (let [A (policy S (action-f S) data eps-count)
            evaluator (bootstrap-eval sarsa-eval
                                      #(terminal? (:state %))
                                      identity)]
        (evaluator (->Pair S A) data eps-count)))))

(defn sarsa-1<- [channel
                 policy
                 action-f
                 reward-f
                 transition-f
                 terminal?]
  "Analogous to `sarsa-1`. It accepts an additional channel, where it writes its intermediate Q-Values"
  (let [sarsa-evl (sarsa-1-eval policy action-f reward-f transition-f)]
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
    (map-vals
      (fn [A R]
        (let [E (C data S A)]
          (+ R (* α δ E)))) As)))

(defn E-sarsa-λ [data S]
  (let [γ (:gamma data)
        λ (:lambda data)
        As (get-in data [:counts S])]
    (map-vals
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
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
  it returns a closure. The closure will, given a start state, start data and current episode count,
  applies SARSA-λ and returns the learned Q-Values"
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
  "Analogous to `sarsa-λ`. It accepts an additional channel, where it writes its intermediate Q-Values"
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
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
  it returns a closure. The closure will, given a start state, start data and current episode count,
  applies Q-Learning and returns the learned Q-Values "
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
  "Analogous to `q-learning`. It accepts an additional channel, where it writes its intermediate Q-Values"
  (let [greedy-policy (greedy find-greedily)
        q-eval (q-learning-eval policy greedy-policy action-f reward-f transition-f)]
    (bootstrap-eval channel
                    q-eval
                    terminal?
                    identity)))

(defn sarsa-max
  "Given a behaviour policy and the action, reward, transition and terminal functions,
  (alternatively also a channel)
  it returns a closure. The closure will, given a starting state, starting data and episode
  count, apply Q-learning with the predefined max greedy policy"
  ([policy
    action-f
    reward-f
    transition-f
    terminal?]
   (q-learning greedy-by-max policy action-f reward-f transition-f terminal?))
  ([channel
    policy
    action-f
    reward-f
    transition-f
    terminal?]
   (q-learning<- channel greedy-by-max policy action-f reward-f transition-f terminal?)))

;; ========= Learned path =========

;; There isn't `always` a greedy path to follow.
;; The occurrence probability of this is inversely proportional to the number of episodes.
;; To avoid the initial pitfall of circulating between states that greedily reference each other,
;; we start to exclude the actions that lead to them the moment we encounter a cycle.

(defn cycled? [path S]
  (some #(= (:state %) S) path))

(defn go-back [find-greedily data path excluded]
  (let [culprit (last path)
        As (get-in data [:q-values (:state culprit)])
        visited (get excluded (:state culprit))
        filtered (filter-kv
                   (fn [A _]
                     (not (some #(= % A) visited))) As)]
    (if (not (empty? filtered))
      (let [A (find-greedily filtered)]
        [(vec (drop-last path))
         (update excluded (:state culprit) #(conj % A))
         (->Pair (:state culprit) A)])
      (go-back find-greedily
               data
               (vec (drop-last path))
               (update excluded (:state culprit) #(conj % (:action culprit)))))))

(defn go-greedy [find-greedily trans-f reward-f terminal?]
  (fn [start data]
    (loop [excluded {}
           path (tuples/tuple)
           S start]
      (if (terminal? S)
        path
        (if (cycled? path S)
          (let [[rem-path n-excluded pair] (go-back find-greedily data path excluded)
                R (reward-f (:state pair) (:action pair))
                S' (trans-f (:state pair) (:action pair))]
            (recur n-excluded
                   (conj rem-path (->Sample (:state pair) (:action pair) R))
                   S'))
          (let [A (-> data (get-in [:q-values S]) (find-greedily))
                R (reward-f S A)
                S' (trans-f S A)]
            (recur (update excluded S #(conj % A))
                   (conj path (->Sample S A R))
                   S')))))))

(defn compute-path
  "Computes the greedy path from given Q-Values.
  Given the transtion, reward and terminal functions, it returns a closure.
  The closure will, given a starting state and some learned Q-Values, calculate the greedy path through
  the Q-Values and return the most `optimal` Markov Chain it could derive from the Q-Values."
  ([transition-f reward-f terminal?]
   (compute-path greedy-by-max transition-f reward-f terminal?))
  ([find-greedily transition-f reward-f terminal?]
   (go-greedy find-greedily transition-f reward-f terminal?)))