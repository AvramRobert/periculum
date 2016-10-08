(ns periculum.genetic
  (:require [periculum.more :as m]
            [clj-tuple :as t]
            [clojure.core.async :as async])
  (:import (java.util Random)))

(comment
  "Evaluation
  { :attempt _
    :score   _
    :errors  _ }")

(defrecord Eval [individual score errors])

(defn map-error [eval f]
  (update-in eval [:errors] f))

(defn map-score [eval f]
  (update-in eval [:score] f))

(defn map-indv [eval f]
  (update-in eval [:individual] f))

(defn lift-eval [item] (->Eval item 0 (t/tuple)))

(defn- non-det [a f g]
  "Applies function `f` or `g` to `a` non-deterministically."
  (let [rnd (new Random)]
    (if (.nextBoolean rnd) (f a) (g a))))

(defn- repopulate [mutate cross amount elite]
  "Creates a new population, based on some pre-existing elite, by
  appling mutation and crossover non-deterministically until the given
  `amount` of individuals have been created."
  (reduce
    (fn [civ _]
      (non-det elite
               #(into civ (cross (rand-nth %) (rand-nth %)))
               #(conj civ (mutate (rand-nth %)))))
    (map :individual elite) (range 0 amount)))

(defn evolve [init fitness mutate cross perfect?]
  "A simple function that runs a genetic algorithm.
  This function essetially receives every meaningful part of a
  genetic algorithm (i.e. mutation, crossover, fitness etc) as a parameter.
  Given these, plus a starting population and a way to find the perfect individual,
  it returns a function, which accepts a number of generations and elites.
  This function is then applied to run the algorithm.

  The provided parameters are of the following type:
    Init: sequence of some `A`
    Mutation: function of A -> A,
    Cross: function of (A, A) -> (A, A)
    Fitness: function of A -> Eval
    Perfect: function of A -> Boolean"

  (let [indv# (count init)]
    (fn [gen# elite#]
      (loop [generation gen#
             civilization init]
        (let [fitted (map #(-> % (lift-eval) (fitness)) civilization)
              best (m/find-some perfect? fitted)]
          (cond
            (some? best) (t/tuple (:individual best) (- gen# generation))
            (> generation 0) (->> fitted
                                  (sort-by :score)
                                  (take elite#)
                                  (repopulate mutate cross indv#)
                                  (recur (dec generation)))
            :default (->> fitted
                          (sort-by :score)
                          (first))))))))


(defn repopulate2 [mutate cross amount elite]
  (reduce
    (fn [population _]
      (let [i1 (rand-nth elite)
            i2 (rand-nth elite)
            [c1 c2] (cross i1 i2)
            [m1 m2] (t/tuple (mutate (lift-eval c1)) (mutate (lift-eval c2)))]
        (conj population c1 c2 m1 m2))) (map :individual elite) (range 0 amount)))

(defn repopulate3 [mutate cross amount elite]
  (loop [npop (t/tuple)]
    (if (>= (count npop) amount)
      npop
      (let [i1 (rand-nth elite)
            i2 (rand-nth elite)
            [c1 c2] (cross i1 i2)
            [m1 m2] (t/tuple (mutate (lift-eval c1)) (mutate (lift-eval c2)))
            all (shuffle (t/tuple c1 c2 m1 m2))]
        (recur (into npop (take (- amount (count npop)) all)))))))

(defn xevolve [init fitness mutate cross perfect?]
  (let [indv# (count init)]
    (fn [gen# elite# f]
      (loop [generation gen#
             civilization init]
        (let [fitted (map #(-> % (f) (lift-eval) (fitness)) civilization)
              best (m/find-some perfect? fitted)]
          (cond
            (some? best) (t/tuple best (- gen# generation))
            (> generation 0) (->> fitted
                                  (sort-by :score)
                                  (take elite#)
                                  (repopulate3 mutate cross indv#)
                                  (recur (dec generation)))
            :default (->> fitted
                          (sort-by :score)
                          (first))))))))
