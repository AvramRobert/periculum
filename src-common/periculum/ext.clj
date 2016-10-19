(ns periculum.ext
  (require [clj-tuple :as t]
           [periculum.more :as m]
           [periculum.rl :as rl]
           [clojure.set :as s]))

(comment
  "A slight problem.
  When two agents exchange information,
  one might receive knowledge about states, to which
  he doesn't have additional information.
  For example agent1 told agent2 that state S and action A have
  some expected reward of R. Agent2 however, doesn't have
  information about the state S', that follows after S-A.
  In this case, he blindly chooses SA, transitions to S',
  looks at his experience for information on S' and finds
  none. He then cannot compute his next action, because he doesn't
  known any for that state.
  => Additive homology is fairly flawed in this type of exchange.
  I must find a way to go about this problem.

  The solution here is to exchange complete subtrees of information.
  The agents choose either a common state s (or two random ones),
  and compute the trajectories from that state until the end. For each
  SA in the path, the agent's expected reward (the one from his experience)
  is also associated with the pair. They then exchange these trajectories.
  The advantage here is that both the additive and homologous properties
  of the crossover are still there. The information conveyed is also complete,
  because they exchange complete sequences of SAR's.
  This is also an instance of a special type of crossover called \"cut and slice \".

  =====================
  Another thing to take into account is the relative number of
  episodes all the agents cover.
  Given that each agent advances only n episodes / generation,
  a number of 10 agents would advance with 10 * n / generation,
  albeit at the same pace. The normal agent accumulates information
  sequentially, whilst the others both sequentially and in parallel.
  With m agents, the convergence can occur after y (singular) number of episodes.
  For genesis, the real number of convergence is collective. Whilst
  it may take a singluar amount of n episodes to converge (n < y),
  the collective amount is #agents * n. This implies more computational cost")

(defn- when-more [g f]                                      ;; make macro
  (if (empty? (-> g :q-values keys)) g (f g)))

(defn- to-sample [genome pair]
  (rl/->Sample (:state pair)
               (:action pair)
               (get-in genome [:q-values (:state pair) (:action pair)])))


;; ========= Value function =========

(defn- vf-cross [follow choose]
  (fn [genome1 genome2]
    (let [[s1 s2] (choose (:q-values genome1) (:q-values genome2))
          s1->se (map #(to-sample genome1 %) (follow s1 genome1))
          s2->se (map #(to-sample genome2 %) (follow s2 genome2))]
      (t/tuple
        (reduce #(assoc-in %1 [:q-values (:state %2) (:action %2)] (:reward %2)) genome1 s2->se)
        (reduce #(assoc-in %1 [:q-values (:state %2) (:action %2)] (:reward %2)) genome2 s1->se)))))

(defn vf-mutate [scalar]
  (fn [genome]
    (when-more genome
               (fn [g]
                 (let [locus (-> g :q-values keys rand-nth)
                       allele (-> g :q-values (get locus) keys rand-nth)]
                   (update-in g [:q-values locus allele] #(+ scalar %)))))))

(defn vf-cross-rnd [follow]
  (letfn [(choose [g1 g2] (t/tuple (-> g1 keys rand-nth)
                                   (-> g2 keys rand-nth)))]
    (vf-cross follow choose)))

(defn vf-eval [start follow]
  (fn [genome]
    (if (empty? (:q-values genome))
      0
      (->> genome
           (follow start)
           (rl/total-reward)))))

;; ========= Planning =========