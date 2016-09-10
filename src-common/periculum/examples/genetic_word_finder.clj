(ns periculum.examples.genetic_word_finder
  (:use periculum.genetic)
  (:require [clj-tuple :as t]))

;; An example usage of the Genetic API.

;; The (not really a) problem solved here is that of a word finder.
;; Given an input of some word, the algorithm genetically attempts to
;; find out what word has been input.
;; This is done by using the input word to construct the fitness function,
;; which in turn evaluates each attempted individual.

;; Test

(defn char-stream [start]
  (iterate #(-> % (int) (inc) (char)) start))

(def caps-alphabet (take 26 (char-stream \A)))
(def non-caps-alphabet (take 26 (char-stream \a)))
(def alphabet (concat caps-alphabet non-caps-alphabet [\space]))

(defn babel [length]
  (vec (map (fn [_] (rand-nth alphabet)) (range 0 length))))

(defn- mutate [evaluatee]
  (let [ridx (rand-nth (:errors evaluatee))]
    (map-indexed
      (fn [idx letter]
        (if (= idx ridx) (rand-nth alphabet) letter)) (:individual evaluatee))))

(defn- cross [evaluatee1 evaluatee2]
  (let [section (rand-int (count (:individual evaluatee1)))
        fuse (fn [amount from to] (into (drop amount (:individual to)) (take amount (:individual from))))]
    (t/tuple
      (fuse section evaluatee1 evaluatee2)
      (fuse section evaluatee2 evaluatee1))))

(defn- evaluator [word]
  (fn [evaluatee]
    (second
      (reduce
        (fn [[idx ev] letter]
          (if (= letter (nth (:individual ev) idx))
            (t/tuple (inc idx) ev)
            (t/tuple (inc idx)
                     (-> ev
                         (map-score inc)
                         (map-error #(conj % idx)))))) (t/tuple 0 evaluatee) word))))

(defn- perfect? [eval] (= (:score eval) 0))

(defn- population [word]
  (let [letter# (count word)
        start (babel letter#)]
    (iterate (fn [_] (babel letter#)) start)))


(defn run-word-finder [word indv# gen#]
  "Given a word, a number of individuals per generation and number of generations,
  it runs a genetic algorithm and tries to find out what word has been input.
  This returns a tuple of the word, and the generation it has been found at."
  (let [elite# 10
        fitness (evaluator word)
        init (take indv# (population word))
        genesis (evolve init fitness mutate cross perfect?)]
    (genesis gen# elite#)))