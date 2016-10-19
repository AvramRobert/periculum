(ns periculum.rl
  (:require [clojure.core.async :as async]
            [clj-tuple :as t]
            [flatland.useful.seq :as s]
            [periculum.more :as m]
            [periculum.genetic :as g])
  (:use [periculum.more]))

(def empty-data {:q-values (t/hash-map)
                 :counts   (t/hash-map)
                 :gamma    0.0
                 :alpha    0.0
                 :lambda   0.0
                 :model    (t/hash-map)})

(defrecord Pair [state action])
(defrecord Sample [state action reward])

(defn- close-all! [channels]
  (if (empty? channels)
    (println "Successfully closed all channels")
    (let [chan (first channels)
          _ (async/close! chan)]
      (recur (rest channels)))))

(defn- send! [data episode channel]
  "Sends `data` asynchronously through the channel"
  (async/go
    (async/>! channel {:cpu     (System/nanoTime)
                       :episode episode
                       :data    data})))
(defn- dispatch!
  "For every dispatch, it sends `data` according to its provided
  schedule. It can additionally accept an endomorphic function `f`,
   that is called on `data` and is evaluated only by need."
  ([data episode dispatches]
   (dispatch! identity data episode dispatches))
  ([f data episode dispatches]
   (when (not-empty? dispatches)
     (foreach
       (fn [[channel p]]
         (when (p episode) (send! (f data) episode channel))) dispatches))))

(defn- show-episode! [episode interval]
  (when (zero? (mod episode interval)) (println episode)))

(defn control->
  "Given a RL-Algorithm closure, some starting configuration and a number of channels, it returns a closure.
  The closure will, given a starting state and a number of episodes, run the algorithm
  for that amount of episodes in a separate thread and echo the intermediate Q-Values in the provided channels."
  ([algorithm config]
   (control-> algorithm config (t/tuple)))
  ([algorithm config dispatches]
   (fn [start eps]
     (async/thread
       (println "Executing")
       (letfn [(destroy-return! [passing]
                 (close-all! (map first dispatches))
                 passing)]
         (->>
           (range 1 eps)
           (reduce
             (fn [data episode]
               (show-episode! episode 100)
               (dispatch! data episode dispatches)
               (algorithm data start episode)) config)
           (destroy-return!)
           (spyr (fn [_] (println "Done")))))))))

(defn control->gen
  "Very similar to `control->`, with the addition, that it can accept
  an evolutionary algorithm for optimising a population of RL-agents.
  The configuration also additional necessitates an `interval`, stating
  at which episode the optimisiation should occur, and a `population`,
  stating how many agents should run in parallel.

  `evolve` is the evolution function, accepting all Q-values and
  returning an evolved population of Q-values.
  `fittest` finds the fittest of the agents and returns him"
  ([algorithm config evolve fittest]
   (control->gen algorithm config evolve fittest))
  ([algorithm config evolve fittest dispatches]
   (fn [start eps]
     (async/thread
       (println "Executing")
       (letfn [(genesis [episode civ]
                 (if (and (> episode 1)
                          (zero? (mod episode (:interval config))))
                   (evolve civ) civ))
               (destroy-return! [passing]
                 (close-all! (map first dispatches))
                 passing)]
         (->>
           (range 1 eps)
           (reduce
             (fn [p episode]
               (show-episode! episode 100)
               (dispatch! fittest p episode dispatches)
               (->> p (pmap #(algorithm % start episode)) (genesis episode)))
             (repeat (:population config) config))
           (fittest)
           (destroy-return!)
           (spyr (fn [_] (println "Done")))))))))

;; ========= Utils =========

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

(defn econf
  ([gamma population interval generations]
   (econf gamma 0.0 0.0 population interval generations))
  ([gamma alpha population interval generations]
   (econf gamma alpha 0.0 population interval generations))
  ([gamma alpha lambda population interval generations]
   (assoc empty-data :gamma gamma
                     :alpha alpha
                     :lambda lambda
                     :population population
                     :interval interval
                     :generations generations)))


(defn action-mean [As]
  "Means the number of actions"
  (let [sum (reduce (fn [p [_ v]] (+ p v)) 0 As)]
    (/ sum (count As))))

(defn total-reward [chain]
  "Sums up all the rewards within a Markov Chain"
  (reduce #(+ %1 (:reward %2)) 0.0 chain))

(defn Gt [chain]
  "Calculates the expected reward for a chain of given `Samples`"
  (consume
    (fn [sample rem]
      (->> rem
           (total-reward)
           (assoc sample :reward))) chain))

(defn trajectory [policy
                  action
                  reward
                  transition]
  "Function that returns a lazy episode generating function.
  The function can create an indefinite sequence of discounted `Sample`s."
  (fn [data start episode]
    (s/unfold
      (fn [[S idx]]
        (let [A (policy S (action S) data episode)
              R (* (reward S A) (Math/pow (:gamma data) idx))
              S' (transition S A)]
          (t/tuple (->Sample S A R) (t/tuple S' (inc idx))))) (t/tuple start 0))))

(defn rollout [policy
               action
               reward
               transition
               terminal?]
  (fn [data start episode]
    (let [roll (s/unfold (fn [S]
                           (let [A (policy S (action S) data episode)
                                 R (reward S A)
                                 S' (transition S A)]
                             (t/tuple (->Sample S A R) S'))) start)]
      (take-while #(not (terminal? (:state %))) roll))))

(defn every-visit-inc
  "Function that increments the count of some observed state-action pair `S, A` each time they
  are observed within a Markov Chain or provided directly"
  ([data chain]
   (reduce (fn [n-data sample]
             (update-in n-data [:counts (:state sample) (:action sample)]
                        #((or-else inc 1) %))) data chain))
  ([data S A]
   (update-in data [:counts S A] #((or-else inc 1) %))))

(defn first-visit-inc [data chain]
  "Function that increments the count of some observed state-action pair `S, A` only the first time they
  are observed within a Markov Chain"
  (->> chain
       (map #(->Pair (:state %) (:action %)))
       (distinct)
       (every-visit-inc data)))

(defn- reset-eligibilities [data]
  "Resets the accumulated eligibilities"
  (assoc data :counts {}))

(defn Q
  "Looks up the Q-Value of a state-action pair"
  ([data SA]
   ((or-else identity 0)
     (get-in data [:q-values (:state SA) (:action SA)])))
  ([data S A]
   ((or-else identity 0)
     (get-in data [:q-values S A]))))

(defn N
  "Looks up the count of a state-action pair"
  ([data SA]
   ((or-else identity 0)
     (get-in data [:counts (:state SA) (:action SA)])))
  ([data S A]
   ((or-else identity 0)
     (get-in data [:counts S A]))))

(defn E-λ [data f]
  "Updates the eligibilities traces"
  (map-vals #(map-vals f %) (:counts data)))

(defn greedy-by-min [As]
  "Given an action-reward representation `{:action1 :reward :action2 :reward ..}`,
  it chooses the action with the smallest expected reward"
  (-> (min-by val As) (keys) (first)))

(defn greedy-by-max [As]
  "Given an action-reward representation `{:action1 :reward :action2 :reward ..}`,
   it chooses the action with the largest expected reward"
  (-> (max-by val As) (keys) (first)))

(defn- evaluate
  "Evaluates one episode of a given RL-algorithm.
  It can be given an addition endofunction, which is applied to the intermediate Q-values
  after each episode."
  ([algorithm terminal?]
   (evaluate algorithm terminal? identity))
  ([algorithm terminal? endo]
   (fn [data state episode]
     (if (terminal? state)
       (endo data)
       (let [[n-data n-init] (algorithm data state episode)]
         (recur n-data n-init episode))))))

(defn- evaluate2
  "Evaluates one episode of a given RL-algorithm.
  It can be given an addition endofunction, which is applied to the intermediate Q-values
  after each episode."
  ([algorithm terminal?]
   (evaluate algorithm terminal? identity))
  ([algorithm terminal? endo]
   (fn [data state episode]
     (if (terminal? state)
       (endo data episode)
       (let [[n-data n-init] (algorithm data state episode)]
         (recur n-data n-init episode))))))


;; ========= Policies =========

(defn- ε-greedy [ε argmax]
  "Concrete implementation of the e-greedy stochastic policy"
  (fn [S As data]
    (if-let [Q-sa (get-in data [:q-values S])]
      (let [p-random (/ ε (.count As))
            p-greedy (+ (- 1.0 ε) p-random)
            greedy-A (argmax Q-sa)]
        (->> As
             (map #(if (= greedy-A %) p-greedy p-random))
             (choose-dist As)))
      (rand-nth As))))

(defn- ε-variant [f argmax]
  "A helper function for creating memoized variants of the ε-greedy policy.
  Here, epsilon itself is somehow calculated by `f` in relation to the current episode.
  Each calculation gets memoized for faster repeated use."
  (let [epsilons (memoize (fn [episode] (f episode)))]
    (fn [S As data episode]
      (-> episode
          (epsilons)                                        ;; lookup or calculate epsilon
          (ε-greedy argmax)                                 ;; create policy
          (apply [S As data])))))                           ;; apply policy

(defn eps-greedy
  "A stochastic policy, where given a probability ε between [0, 1]
  and a function `argmax` for finding the greediest action for some state,
  the agent will choose an exploratory action with a normally distributed
  P(ε) and the greedy action with P(1 - ε). If no `argmax` function is provided,
  it defaults to finding the action with `largest` Q-value"
  ([epsilon]
   (eps-greedy epsilon greedy-by-max))
  ([epsilon argmax]
   (fn [S As data _]
     (-> epsilon                                            ;; take epsilon
         (ε-greedy argmax)                                  ;; create policy
         (apply [S As data])))))                            ;; apply policy

(defn GLIE-eps-greedy
  "An ε-greedy policy, where ε gets reduced by 1/k after each episode.
  k is the current episode count. (ε = ε * 1/k)"
  ([epsilon]
   (GLIE-eps-greedy epsilon greedy-by-max))
  ([epsilon argmax]
   (ε-variant #(* epsilon (/ 1.0 %)) argmax)))

(defn eps-episode
  "An ε-greedy policy, where ε is always preset to 1/k.
  k is the current episode count (ε = 1/k)"
  ([]
   (eps-episode greedy-by-max))
  ([argmax]
   (ε-variant #(/ 1.0 %) argmax)))

(defn eps-balanced
  "A policy, which chooses actions randomly"
  [_ As _ _] (rand-nth As))

(defn greedy
  "A policy, which given a function `argmax` for finding the greediest action for some state,
  always returns this action given that one exists; chooses randomly otherwise"
  ([]
   (greedy greedy-by-max))
  ([argmax]
   (fn [S As data _]
     (if-let [Q-sa (get-in data [:q-values S])]
       (argmax Q-sa)
       (rand-nth As)))))

;; ============ Monte Carlo ============

(defn- Q-monte-carlo [data sample]
  (let [{S     :state
         A     :action
         Gt-SA :reward} sample
        Q-SA (Q data S A)
        N-SA (N data S A)]
    (assoc-in data [:q-values S A] (+ Q-SA (* (/ 1 N-SA) (- Gt-SA Q-SA))))))

(defn- monte-carlo-update [data markov-chain]
  (reduce Q-monte-carlo (every-visit-inc data markov-chain) markov-chain))

(defn- monte-carlo-eval [policy
                         action
                         reward
                         transition
                         terminal?]
  (let [gen-episode (trajectory policy action reward transition)]
    (fn [data S episode]
      (->> (gen-episode data S episode)                     ;; lazy generator
           (take-while #(not (terminal? (:state %))))       ;; generate an episode
           (Gt)                                             ;; calculate expected reward
           (monte-carlo-update data)))))                    ;; update Q-values

(defn monte-carlo [policy
                   action-f
                   reward-f
                   transition-f
                   terminal?]
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
 it returns a closure. The closure will, given a start state, start data and current episode count,
 applies Monte Carlo and return the learned Q-Values"
  (monte-carlo-eval policy action-f reward-f transition-f terminal?))

;; ========= SARSA(1) =========

(defn Q-sarsa-1 [data S A R S' A']
  (let [alpha (:alpha data)
        gamma (:gamma data)
        Q-SA (Q data S A)
        Q-S'A' (Q data S' A')]
    (+ Q-SA (* alpha (+ R (- (* gamma Q-S'A') Q-SA))))))

(defn- sarsa-1-eval [policy
                     action
                     reward
                     transition]
  (fn [data pair episode]
    (let [{S :state
           A :action} pair
          R (reward S A)
          S' (transition S A)
          A' (policy S' (action S') data episode)]
      (-> data
          (assoc-in [:q-values S A] (Q-sarsa-1 data S A R S' A')) ;; update choice
          (t/tuple (->Pair S' A'))))))                      ;; return new data and next pair

(defn sarsa-1 [policy
               action
               reward
               transition
               terminal?]
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
    it returns a closure. The closure will, given a start state, start data and current episode count,
    applies SARSA-1 and returns the learned Q-Values"
  (fn [data S episode]
    (let [A (policy S (action S) data episode)]
      (-> policy
          (sarsa-1-eval action reward transition)
          (evaluate #(terminal? (:state %)))
          (apply [data (->Pair S A) episode])))))


;;  ========= SARSA(λ) =========
(defn- td-error [S A R S' A' data]
  (let [γ (:gamma data)
        Q-S'A' (Q data S' A')
        Q-SA (Q data S A)]
    (+ R (- (* γ Q-S'A') Q-SA))))

(defn- Q-sarsa-λ [data δ]
  (let [α (:alpha data)]
    (merge-with
      #(merge-with (fn [R E] (+ R (* α δ E))) %1 %2) (:q-values data) (:counts data))))

(defn- E-sarsa-λ [data]
  (let [γ (:gamma data)
        λ (:lambda data)]
    (E-λ data (fn [E] (* γ λ E)))))

(defn- sarsa-λ-update [data δ]
  (-> data
      (assoc :q-values (Q-sarsa-λ data δ))
      (assoc :counts (E-sarsa-λ data))))

(defn- sarsa-λ-eval [policy
                     action
                     reward
                     transition]
  (fn [data pair episode]
    (let [{S :state
           A :action} pair
          R (reward S A)
          S' (transition S A)
          A' (policy S' (action S') data episode)
          δ (td-error S A R S' A' data)]
      (-> data
          (update-in [:q-values S A] #((or-else identity 0) %)) ;; init S A in Qs
          (every-visit-inc S A)                             ;; increment eligibility
          (sarsa-λ-update δ)                                ;; blame choices and decay eligibilities
          (t/tuple (->Pair S' A'))))))                      ;; return new data and next pair

(defn sarsa-λ [policy
               action
               reward
               transition
               terminal?]
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
  it returns a closure. The closure will, given a start state, start data and current episode count,
  applies SARSA-λ and returns the learned Q-Values"
  (fn [data S episode]
    (let [A (policy S (action S) data episode)]
      (-> policy
          (sarsa-λ-eval action reward transition)
          (evaluate #(terminal? (:state %)) reset-eligibilities)
          (apply [data (->Pair S A) episode])))))

;; ========= Q-Learning =========

(defn- q-learning-eval [policy
                        greedy-policy
                        action
                        reward
                        transition]
  (fn [data S episode]
    (let [A (policy S (action S) data episode)
          R (reward S A)
          S' (transition S A)
          A' (greedy-policy S' (action S') data episode)]
      (-> data
          (assoc-in [:q-values S A] (Q-sarsa-1 data S A R S' A')) ;; Q-learning is a half greedy Sarsa-1
          (t/tuple S')))))

(defn q-learning [policy
                  greedy-policy
                  action
                  reward
                  transition
                  terminal?]
  "Given a behaviour and greedy policy and the action, reward, transition and terminal functions,
  it returns a closure. The closure will, given a start state, start data and current episode count,
  applies Q-Learning and returns the learned Q-Values"
  (-> policy
      (q-learning-eval greedy-policy action reward transition)
      (evaluate terminal?)))

(defn sarsa-max [policy
                 action
                 reward
                 transition
                 terminal?]
  "Given a behaviour policy and the action, reward, transition and terminal functions,
  it returns a closure. The closure will, given a starting state, starting data and episode
  count, apply Q-learning with the greedy policy of choosing the action with the largest Q-value"
  (q-learning policy (greedy greedy-by-max) action reward transition terminal?))


;; ========= Watkins' Q(λ) =========

(defn- E-q-λ [data A' A*]
  (if (= A' A*) (E-sarsa-λ data)
                (E-λ data (fn [_] 0))))

(defn- q-λ-update [data A' A* δ]
  (-> data
      (assoc :q-values (Q-sarsa-λ data δ))
      (assoc :counts (E-q-λ data A' A*))))

(defn- q-λ-eval [policy
                 greedy-policy
                 action
                 reward
                 transition]
  (fn [data pair episode]
    (let [{S :state
           A :action} pair
          R (reward S A)
          S' (transition S A)
          A' (policy S' (action S') data episode)
          A* (greedy-policy S' (action S') data episode)
          δ (td-error S A R S' A* data)]
      (-> data
          (update-in [:q-values S A] #((or-else identity 0) %))
          (every-visit-inc S A)
          (q-λ-update A' A* δ)
          (t/tuple (->Pair S' A'))))))

(defn q-λ [policy
           greedy-policy
           action
           reward
           transition
           terminal?]
  "Given a behaviour and a greedy policy, the action, reward, transition and terminal functions,
  it returns a closure. The closure will, given a starting state, starting data and episode
  count, apply Watkins' Q(λ) and return the Q-Values"
  (fn [data S episode]
    (let [A (policy S (action S) data episode)]
      (-> policy
          (q-λ-eval greedy-policy action reward transition)
          (evaluate #(terminal? (:state %)) reset-eligibilities)
          (apply [data (->Pair S A) episode])))))

(defn q-λ-max [policy
               action
               reward
               transition
               terminal?]
  "Given a behaviour policy and the action, reward, transition and terminal functions,
 it returns a closure. The closure will, given a starting state, starting data and episode
 count, apply Watkins' Q(λ) with the greedy policy of choosing the action with the largest Q-value"
  (q-λ policy (greedy greedy-by-max) action reward transition terminal?))

;; Model-based RL

;; ========= Dyna-Q =========

(defn- think [data n argmax]
  (reduce
    (fn [ndata _]
      (let [S (-> ndata (:q-values) (keys) (rand-nth))
            A (-> ndata (:q-values) (get S) (keys) (rand-nth))
            {R :reward S' :next} (get-in ndata [:model S A])
            A' (argmax ndata S')]
        (assoc-in data [:q-values S A] (Q-sarsa-1 ndata S A R S' A')))) data (range 0 n)))

(defn- dyna-q-eval [thinking-time
                    policy
                    greedy-policy
                    action
                    reward
                    transition]
  (fn [data S episode]
    (let [A (policy S (action S) data episode)
          R (reward S A)
          S' (transition S A)
          A' (greedy-policy S' (action S') data episode)
          argmax (fn [d state] (greedy-policy state (action state) d episode))]
      (-> data
          (assoc-in [:q-values S A] (Q-sarsa-1 data S A R S' A'))
          (assoc-in [:model S A] (t/hash-map :reward R :next S'))
          (think thinking-time argmax)
          (t/tuple S')))))

(defn dyna-q [thinking-time
              policy
              greedy-policy
              action
              reward
              transition
              terminal?]
  (-> thinking-time
      (dyna-q-eval policy greedy-policy action reward transition)
      (evaluate terminal?)))

(defn dyna-q-max [thinking-time]
  (fn [policy
       action
       reward
       transition
       terminal?]
    (dyna-q thinking-time policy (greedy greedy-by-max) action reward transition terminal?)))

;; ========= Dyna-rho =========

(defn- fuse-at [at g1 g2]
  (m/fuse (take-while #(not (= (:state %) at)) g1)
          (drop-while #(not (= (:state %) at)) g2)))

(defn- candidate [set1 set2]
  (let [x (vec (clojure.set/intersection set1 set2))]
    (if (empty? x) nil (rand-nth x))))


(defn- r-mutate [data rollout]
  (fn [genome]
    (let [sample (rand-nth genome)
          tail (rollout data (:state sample))]
      (fuse-at (:state sample) genome tail))))

(defn- r-cross [genome1 genome2]
  (let [s1 (->> genome1 (drop 1) (map :state) (set))
        s2 (->> genome2 (drop 1) (map :state) (set))]
    (if-let [X (candidate s1 s2)]
      (t/vector
        (fuse-at X genome1 genome2)
        (fuse-at X genome2 genome1))
      (t/vector genome1 genome2))))

(defn- r-eval [genome] (total-reward genome))

(defn- dyna-r-eval [policy
                    argmax
                    action
                    reward
                    transition]
  (fn [data S episode]
    (let [A (policy S (action S) data episode)
          R (reward S A)
          S' (transition S A)
          A' (argmax S' (action S') data episode)]
      (-> data
          (assoc-in [:q-values S A] (Q-sarsa-1 data S A R S' A'))
          (assoc-in [:model S A] (t/hash-map :reward R :next S'))
          (t/tuple S')))))

(defn- rollout-model [terminal?]
  (fn [data start]
    (loop [chain (t/tuple)
           S start]
      (if (terminal? S)
        chain
        (let [A (eps-balanced S (-> data (get-in [:model S]) keys vec) data nil)
              {R  :reward
               S' :next} (get-in data [:model S A])]
          (recur (conj chain (->Sample S A R)) S'))))))

(defn- q-update-sample [argmax
                        action
                        transition]
  (fn [data sample]
    (let [{S :state
           A :action
           R :reward} sample
          S' (transition S A)
          A' (argmax S' (action S') data nil)]
      (assoc-in data [:q-values S A] (Q-sarsa-1 data S A R S' A')))))

(defn- planned-genesis [genesis
                        argmax
                        action
                        transition]
  (fn [data]
    (let [q-update (q-update-sample argmax action transition)]
      (->> data
           (genesis (-> data :model keys vec rand-nth))
           (reduce q-update data)))))

(defn dyna-r [population
              generations
              elites
              roulette
              perfect?]
  (fn [policy
       action
       reward
       transition
       terminal?]
    (let [rollout (rollout-model terminal?)
          genesis (fn [start data]
                    (-> (repeat population (rollout data start))
                        (g/genetically r-eval
                                       (g/selection (g/n-elitism elites)
                                                    (g/roulette roulette))
                                       (r-mutate data rollout)
                                       r-cross
                                       perfect?)
                        (apply [generations])
                        (second)
                        (g/indv)))
          plan (planned-genesis genesis (greedy greedy-by-max) action transition)]
      (-> policy
          (dyna-r-eval (greedy greedy-by-max) action reward transition)
          (evaluate terminal? plan)))))

;; ========= Learned path =========

;; There isn't `always` a greedy path to follow.
;; The occurrence probability of this is inversely proportional to the number of episodes.
;; To avoid the initial pitfall of circulating between states that greedily reference each other,
;; we start to exclude the actions that lead to them the moment we encounter a cycle.

(defn- cycled? [path S]
  (some #(= (:state %) S) path))

(defn- go-back [argmax data path excluded]
  (let [culprit (last path)
        As (get-in data [:q-values (:state culprit)])
        visited (get excluded (:state culprit))
        filtered (filter-kv
                   (fn [A _]
                     (not (some #(= % A) visited))) As)]
    (if (not (empty? filtered))
      (let [A (argmax filtered)]
        [(vec (drop-last path))
         (update excluded (:state culprit) #(conj % A))
         (->Pair (:state culprit) A)])
      (go-back argmax
               data
               (vec (drop-last path))
               (update excluded (:state culprit) #(conj % (:action culprit)))))))

(defn- go-greedy [argmax transition reward terminal?]
  (fn [start data]
    (loop [excluded {}
           path (t/tuple)
           S start]
      (if (terminal? S)
        path
        (if (cycled? path S)
          (let [[rem-path n-excluded pair] (go-back argmax data path excluded)
                R (reward (:state pair) (:action pair))
                S' (transition (:state pair) (:action pair))]
            (recur n-excluded
                   (conj rem-path (->Sample (:state pair) (:action pair) R))
                   S'))
          (let [A (-> data (get-in [:q-values S]) (argmax))
                R (reward S A)
                S' (transition S A)]
            (recur (update excluded S #(conj % A))
                   (conj path (->Sample S A R))
                   S')))))))

(defn compute-path
  "Computes the greedy path from given Q-Values.
  Given the transtion, reward and terminal functions, it returns a closure.
  The closure will, given a starting state and some learned Q-Values, calculate the greedy path through
  the Q-Values and return the most `optimal` Markov Chain it could derive from the Q-Values."
  ([transition reward terminal?]
   (compute-path greedy-by-max transition reward terminal?))
  ([argmax transition reward terminal?]
   (go-greedy argmax transition reward terminal?)))
