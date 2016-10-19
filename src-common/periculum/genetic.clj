(ns periculum.genetic
  (:require [periculum.more :as m]
            [clj-tuple :as t]
            [clojure.core.async]
            [flatland.useful.seq :as f])
  (:import (java.util Random)))

(comment
  "Evaluation
  { :individual  _
    :score       _ }")

(defrecord Eval [index individual score])

(defn lift-eval
  ([item] (lift-eval item 0))
  ([item score] (->Eval 0 item score)))

(defn indv [ev] (:individual ev))

(defn- non-det [a f g]
  "Applies function `f` or `g` to `a` non-deterministically."
  (let [rnd (new Random)]
    (if (.nextBoolean rnd) (f a) (g a))))

(defn- degressive [f amount evaluated]
  "Degressively chooses `amount` individuals from `evaluated` using `f`"
  (loop [parents (t/tuple)
         elder evaluated]
    (cond
      (empty? elder) parents
      (>= (count parents) amount) parents
      :default
      (let [candidate (f elder)]
        (recur (conj parents candidate)
               (remove #(= (:index %) (:index candidate)) elder))))))

(defn- roulette-select [evaluated]
  "Selects an individual by means of roulette-wheel selection"
  (let [reds (reductions #(+ %1 (:score %2)) 0.0 evaluated)
        r (rand-int (last reds))]
    (->> evaluated
         (f/zip reds)
         (drop-while (fn [[v, _]] (< v r)))
         (first)
         (second))))

(defn- tournament-select [k evaluated]
  "Selects an individual by means of tournament selection."
  (let [candidates (t/tuple (rand-nth evaluated) (rand-nth evaluated))]
    (if (> k (rand)) (m/max-by :score candidates)
                     (m/min-by :score candidates))))

(defn- elite-select [f evaluated]
  "Selects the fittest individual by means of `f`"
  (->> evaluated
       (m/desc-by :score)
       (f)))

(defn roulette [amount]
  "Given an amount, it returns a function, that when given an evaluated population,
  chooses the given amount of individuals based on roulette-wheel selection."
  (fn [evaluated]
    (degressive roulette-select amount evaluated)))

(defn tournament [k amount]
  "Given a `k` factor between [0, 1] and an amount, it returns a function, that when given an evaluated
  population, chooses `amount` number of individuals based on tournament selection."
  (fn [evaluated]
    (degressive #(tournament-select k %) amount evaluated)))

(defn n-elitism [amount]
  "Given an amount, it returns a function, that when given an evaluated population,
  chooses the given amount of fittest individuals."
  (fn [evaluated]
    (elite-select #(take amount %) evaluated)))

(defn selection [& fs]
  "Given an evaluated population and a number of selection functions,
  it selects degressively and incrementally with each given function.
  Degressive means, that once one individual was selected, it is no longer
  present in the initial selection pool."
  (fn [evaluated]
    (let [total (count evaluated)]
      (loop [elder evaluated
             parents (t/tuple)
             [f & tfs] fs]
        (cond
          (empty? elder) parents
          (nil? f) parents
          (>= (count parents) total) parents
          :default
          (let [chosen (f elder)
                idxs (map :index chosen)]
            (recur (remove #(f/include? (:index %) idxs) evaluated)
                   (m/fuse chosen parents)
                   tfs)))))))

(defn genesis [mutate crossover]
  "Given a mutation and crossover function, it returns a function
  that can recreate a population, based on some pre-existing elite, by
  applying crossover first, and then mutation until a given `amount`
  of individuals have been created. This is the standard genetic reproduction."
  (fn [amount parents]
    (loop [newborn parents]
      (if (>= (count newborn) amount)
        newborn
        (let [[p1 p2] (t/tuple (rand-nth parents) (rand-nth parents))
              [c1 c2] (crossover p1 p2)
              [m1 m2] (t/tuple (mutate c1) (mutate c2))]
          (recur (conj newborn c1 c2 m1 m2)))))))

(defn b-genesis [mutate cross]
  "Given a mutation and crossover function, it returns a function
  that can recreate a population, based on some pre-existing elite, by
  appling either mutation and crossover non-deterministically until a given
  `amount` of individuals have been created."
  (fn [amount population]
    (reduce
      (fn [civ _]
        (non-det population
                 #(into civ (cross (rand-nth %) (rand-nth %)))
                 #(conj civ (mutate (rand-nth %))))) population (range 0 amount))))

(defn evolve
  "A function that runs an evolutionary algorithm.
  Every meaningful part of this function is provided as a parameter.
  It receives a starting population, a fitness function and a repopulation function.
  Additionally, it may receive a predicate, that is able to find a perfect individual.
  Given these, it returns a new function, which computes the algorithm given
  a number of generations and of elites.

  Parameters:
    population -> starting individuals
    fitness -> computes the fitness of an individual. It receives an individual and
               returns a number representing that individual's fitness (or score).
               Larger numbers are associated with better fitness.
    repopulate -> recreates the population. It takes an amount of individuals
                  and the selected elite and must return a new collection of individuals.


    Note: `repopulate` abstracts over the type of evolutionary algorithm used.
    It knows how and which genetic operators to use. It can also
    do other things, if it is so desired. It must however be pure."

  ([population fitness select repopulate]
   (evolve population fitness select repopulate (fn [_] false)))
  ([population fitness select repopulate perfect?]
   (let [indv# (count population)]
     (fn [gen#]
       (loop [generation gen#
              individuals population]
         (let [fitted (map-indexed (fn [idx itm] (->Eval idx itm (fitness itm))) individuals)
               best (m/find-some perfect? fitted)]
           (cond
             (some? best) (t/tuple (- gen# generation) best)
             (> generation 0) (->> fitted
                                   (m/desc-by :score)
                                   (select)
                                   (map indv)
                                   (filter #(not (nil? %)))
                                   (repopulate indv#)
                                   (recur (dec generation)))
             :default (->> fitted
                           (m/desc-by :score)
                           (first)
                           (t/tuple gen#)))))))))


(defn evolve-w [population fitness select repopulate]
  "A function that runs an evolutionary algorithm.
  Similar to `evolve`, it evolves by using the same schema, but
   does not look for a `perfect` individual and also does not
   return one single individual. It returns the whole evolved population
   after the specified generations have elapsed."
  (let [indv# (count population)]
    (fn [gen#]
      (loop [generation gen#
             individuals population]
        (if (> generation 0)
          (->> individuals
               (map-indexed (fn [idx itm] (->Eval idx itm (fitness itm))))
               (m/desc-by :score)
               (select)
               (map indv)
               (filter #(not (nil? %)))
               (repopulate indv#)
               (recur (dec generation)))
          (->> individuals
               (map-indexed (fn [idx itm] (->Eval idx itm (fitness itm))))
               (m/desc-by :score)
               (take indv#)
               (map indv)))))))

(defn genetically [population fitness select mutate cross perfect?]
  (evolve population fitness select (genesis mutate cross) perfect?))

(defn b-genetically [population fitness select mutate cross perfect?]
  (evolve population fitness select (b-genesis mutate cross) perfect?))
