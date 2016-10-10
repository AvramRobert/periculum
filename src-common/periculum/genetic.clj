(ns periculum.genetic
  (:require [periculum.more :as m]
            [clj-tuple :as t]
            [clojure.core.async])
  (:import (java.util Random)))

(comment
  "Evaluation
  { :individual  _
    :score       _ }")

;; FIXME: Implement roulette and tournament selection

(defrecord Eval [individual score])

(defn lift-eval
  ([item] (lift-eval item 0))
  ([item score] (->Eval item score)))

(defn indv [ev] (:individual ev))

(defn- non-det [a f g]
  "Applies function `f` or `g` to `a` non-deterministically."
  (let [rnd (new Random)]
    (if (.nextBoolean rnd) (f a) (g a))))

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

  ([population fitness repopulate]
   (evolve population fitness repopulate (fn [_] false)))
  ([population fitness repopulate perfect?]
   (let [indv# (count population)]
     (fn [gen# elite#]
       (loop [generation gen#
              individuals population]
         (let [fitted (map #(->Eval % (fitness %)) individuals)
               best (m/find-some perfect? fitted)]
           (cond
             (some? best) (t/tuple (- gen# generation) best)
             (> generation 0) (->> fitted
                                   (m/desc-by :score)
                                   (take elite#)
                                   (map indv)
                                   (repopulate indv#)
                                   (recur (dec generation)))
             :default (->> fitted
                           (m/desc-by :score)
                           (first)
                           (t/tuple gen#)))))))))


(defn evolve-w [population fitness repopulate]
  "A function that runs an evolutionary algorithm.
  Similar to `evolve`, it evolves by using the same schema, but
   does not look for a `perfect` individual and also does not
   return one single individual. It returns the whole evolved population
   after the specified generations have elapsed."
  (let [indv# (count population)]
    (fn [gen# elite#]
      (loop [generation gen#
             individuals population]
        (if (> generation 0)
          (->> individuals
               (map #(->Eval % (fitness %)))
               (m/desc-by :score)
               (take elite#)
               (map indv)
               (repopulate indv#)
               (recur (dec generation)))
          (->> individuals
               (map #(->Eval % (fitness %)))
               (m/desc-by :score)
               (take indv#)
               (map indv)))))))

(defn genetically [population fitness mutate cross perfect?]
  (evolve population fitness (genesis mutate cross) perfect?))

(defn b-genetically [population fitness mutate cross perfect?]
  (evolve population fitness (b-genesis mutate cross) perfect?))
