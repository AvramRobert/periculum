(ns periculum.examples.genetic_word_finder
  (:use periculum.genetic)
  (:require [clj-tuple :as t]
            [flatland.useful.seq :as f]
            [periculum.more :as m]))

;; An example usage of the Genetic API.

;; The (not really a) problem solved here is that of a word finder.
;; Given an input of some word, the algorithm genetically attempts to
;; find out what word has been input.
;; This is done by using the input word to construct the fitness function,
;; which in turn evaluates each attempted individual.

(defn char-stream [start]
  (iterate #(-> % (int) (inc) (char)) start))

(def caps-alphabet (take 26 (char-stream \A)))
(def non-caps-alphabet (take 26 (char-stream \a)))
(def alphabet (concat caps-alphabet non-caps-alphabet [\space]))

(defn babel [length]
  (vec (map (fn [_] (rand-nth alphabet)) (range 0 length))))

(defn- mutate [genome]
  (let [ridx (rand-int (count genome))]
    (map-indexed
      (fn [idx letter]
        (if (= idx ridx) (rand-nth alphabet) letter)) genome)))

(defn- cross [genome1 genome2]
  (let [section (rand-int (count genome1))]
    (t/tuple
      (m/fuse (take section genome1) (drop section genome2))
      (m/fuse (take section genome2) (drop section genome1)))))

(defn- evaluator [word]
  (fn [genome]
    (reduce
      (fn [score [idx letter]]
        (if (= (nth genome idx) letter)
          score
          (dec score))) 0 (f/indexed word))))

(defn- perfect? [eval] (= (:score eval) 0))

(defn- population [word]
  (let [letter# (count word)
        start (babel letter#)]
    (iterate (fn [_] (babel letter#)) start)))

(defn run-word-finder [word indv# gen# elite#]
  "Given a word, a number of individuals per generation and number of generations,
  it runs a genetic algorithm and tries to find out what word has been input.
  This returns a tuple of the word, and the generation it has been found at."
  (let [fitness (evaluator word)
        init (take indv# (population word))
        genesis (genetically init fitness mutate cross perfect?)]
    (genesis gen# elite#)))