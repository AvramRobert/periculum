(ns periculum.ext
  (require [clj-tuple :as t]
           [periculum.more :as m]
           [periculum.domain :as d]
           [periculum.rl :as rl]
           [periculum.genetic :as g]
           [clojure.set :as s]))

;; Cross can be both additive and subtractive
;; If given sub_e1, sub_e2 and |sub_e1| > |sub_e2|
;; |cross sub_e1 sub_e2| = |sub_e1|
;; |cross sub_e2 sub_e1| = |sub_e1|
;; => the larger one always keeps its structure, regardless of
;; the amount of elemets he is forced to swap with
;; Structure is not subtracted from it, if the its sharing
;; partner has less information than him
;; => this means that it's associative
;; and keep it if

(comment
  "A slight problem.
  When two agents exchange information,
  one might receive knowledge about states, to which
  he doesn't have additional information.
  For example agent1 told agent2 that state S and action A have
  some expected reward of R. Agent2 however, doesn't have
  information about the state S', that follows after S-A.
  In this case, he blindly chooses SA, transitions to S',
  looks into his experience for information on S' and finds
  none. He then cannot compute his next action, because he doesn't
  known any for that state.
  => Additive homology is fairly flawed in this regard.
  I must find a way to convey go about this problem.
  One scenario might be to just re-route him to the same state.
  When following his policy greedily, if this situation occurs,
  he is simply re-routed to the same state. The resolver function
  will then take care of this loop, make him go back and
  choose something else.
  This is however not really ideomatic. I need to find something else.

  =====================
  Another thing to take into account is the relative number of
  episodes all the agents cover.
  Given that each agent advances only n episodes / generation,
  a number of 10 agents would advance with 10 * n / generation,
  but all at the same pace. The normal agent accumulates information
  sequentially, whilst the others both sequentially and in parallel.
  I need to then try to normalise these, in order to better
  be able to view them. With 1 agent, the number of episodes is
  x until an optimum is achieved. With m agents, the optimum is
  after y (singular) number of episodes. For genesis,
  the real number (collective number) is y * #agents")

(defn- merge-within [f this that]
  (merge-with
    #(merge-with
      (fn [a b] (f a b)) %1 %2) this that))

(defn- intersect-within [f this that]
  (m/intersect-with
    #(m/intersect-with
      (fn [a b] (f a b)) %1 %2) this that))

(defn- merge-left [this that]
  (merge-within (fn [a _] a) this that))

(defn- merge-right [this that]
  (merge-within (fn [_ b] b) this that))

(defn- intersect-right [this that]
  (intersect-within (fn [_ b] b) this that))

(defn when-more [g f]                                       ;; make macro
  (if (empty? (-> g :q-values keys)) g (f g)))

;; ========= Value function =========

(defn- serialise [genome]
  (reduce
    (fn [coll [k v]]
      (into coll (->> (repeat k)
                      (take (count v))
                      (map #(flatten [%2 %1]) (keys v))))) [] (:q-values genome)))

(defn- vf-single-cross [at genome1 genome2]
  (if (empty? (-> genome1 :q-values))
    (t/tuple genome1 genome2)
    (let [sub (->> genome1 :q-values (take at) (flatten) (apply t/hash-map))]
      (t/tuple
        (assoc genome1 :q-values (merge-right (:q-values genome1) (intersect-right sub (:q-values genome2))))
        (assoc genome2 :q-values (merge-left sub (:q-values genome2)))))))

(defn- vf-double-cross [p0 p1 genome1 genome2]
  (if (empty? (-> genome1 :q-values))
    (t/tuple genome1 genome2)
    (let [sub (->> genome1 :q-values (drop p0) (take (- p1 p0)) (flatten) (apply t/hash-map))]
      (t/tuple
        (assoc genome1 :q-values (merge-right (:q-values genome1) (intersect-right sub (:q-values genome2))))
        (assoc genome2 :q-values (merge-left sub (:q-values genome2)))))))

(defn- vf-mutate [scalar genome]
  (when-more genome
             (fn [g]
               (let [locus (-> g :q-values keys rand-nth)
                     allele (-> g :q-values (get locus) keys rand-nth)]
                 (update-in g [:q-values locus allele] #(+ scalar %))))))

(defn vf-cross-1 [evaluatee1 evaluatee2]
  (-> evaluatee1
      :individual
      :q-values
      count
      rand-int
      (vf-single-cross (:individual evaluatee1) (:individual evaluatee2))))

(defn vf-cross-2 [evaluatee1 evaluatee2]
  (let [n (count (-> evaluatee1 :individual :q-values))
        p0 (rand-int n)
        x (rand-int n)
        p1 (if (> p0 x) (+ x p0) x)]
    (vf-double-cross p0 p1 (:individual evaluatee1) (:individual evaluatee2))))

(defn vf-mutate-a [scalar]
  (fn [evaluatee]
    (vf-mutate scalar (:individual evaluatee))))

(defn vf-eval [start world]
  (let [terminal? (d/terminal? world)
        reward (d/reward world terminal?)
        transition (d/transition world terminal?)
        fitness (rl/compute-path transition reward terminal?)]
    (fn [evaluatee]
      (if (empty? (-> evaluatee :individual :q-values))
        evaluatee
        (g/map-score evaluatee (fn [_]
                              (->> evaluatee
                                   :individual
                                   (fitness start)
                                   (reduce #(+ %1 (:reward %2)) 0))))))))

;; ========= Planning =========

(defn- fuse-at [at g1 g2]
  (flatten
    (vector
      (take-while #(not (= (:state %) at)) g1)
      (drop-while #(not (= (:state %) at)) g2))))

(defn- candidate [set1 set2]
  (let [x (vec (s/intersection set1 set2))]
    (if (empty? x) nil (rand-nth x))))

(defn p-cross-1 [genome1 genome2]
  (let [s1 (->> genome1 (drop 1) (map :state) (set))
        s2 (->> genome2 (drop 1) (map :state) (set))]
    (if-let [X (candidate s1 s2)]
      (t/vector
        (fuse-at X genome1 genome2)
        (fuse-at X genome2 genome1))
      (t/vector genome1 genome2))))

(defn- roll [actions transition terminal?]
  (fn [state]
    (loop [S state traj (t/vector)]
      (if (terminal? S)
        traj
        (let [A (rl/eps-balanced S (actions S) nil nil)
              S' (transition S A)]
          (recur S' (conj traj (rl/->Pair S A))))))))

;; Mutation can actually be informed
;; Perhaps use his own experience as a guide
(defn p-mutate [rollout genome]
  (let [pair (rand-nth genome)
        ntail (rollout (:state pair))]
    (fuse-at (:state pair) genome ntail)))

(defn p-evaluator [reward]
  (fn [genome]
    (g/map-score
      genome
      (fn [_]
        (reduce #(+ %1 (reward (:state %2) (:action %2))) 0 genome)))))
