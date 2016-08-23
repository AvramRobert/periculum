(ns periculum.genetic
  (:require [periculum.more :as m]
            [clj-tuple :as t])
  (:import (java.util Random)))

(defrecord Eval [individual score errors])

(defn lift-eval [item] (->Eval item 0 (t/tuple)))

(defn non-det [a f g]
  (let [rnd (new Random)]
    (if (.nextBoolean rnd) (f a) (g a))))

(defn repopulate [mutate cross amount rem]
  (reduce
    (fn [civ _]
      (non-det rem
               #(into civ (cross (rand-nth %) (rand-nth %)))
               #(conj civ (mutate (rand-nth %)))))
    (map :individual rem) (range 0 amount)))

(comment
  "Evaluation
  { :attempt _
    :score   _
    :errors  _ }")

(defn evolve [init fitness mutate cross perfect?]
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
            :default (first civilization)))))))

(defn map-error [eval f]
  (update-in eval [:errors] f))

(defn map-score [eval f]
  (update-in eval [:score] f))

(defn map-indv [eval f]
  (update-in eval [:individual] f))

;; Test

(def char-stream
  (iterate #(-> % (int) (inc) (char)) \A))

(def alphabet (take 26 char-stream))

(defn babel [length]
  (vec (map (fn [_] (rand-nth alphabet)) (range 0 length))))

(defn mutate [evaluatee]
  (let [ridx (rand-nth (:errors evaluatee))]
    (map-indexed
      (fn [idx letter]
        (if (= idx ridx) (rand-nth alphabet) letter)) (:individual evaluatee))))

(defn cross [evaluatee1 evaluatee2]
  (let [section (rand-int (count (:individual evaluatee1)))
        fuse (fn [amount from to] (into (drop amount (:individual to)) (take amount (:individual from))))]
    (t/tuple
      (fuse section evaluatee1 evaluatee2)
      (fuse section evaluatee2 evaluatee1))))

(defn evaluator [name]
  (fn [evaluatee]
    (second
      (reduce
        (fn [[idx ev] letter]
          (if (= letter (nth (:individual ev) idx))
            (t/tuple (inc idx) ev)
            (t/tuple (inc idx)
                     (-> ev
                         (map-score inc)
                         (map-error #(conj % idx)))))) (t/tuple 0 evaluatee) name))))

(defn perfect? [eval] (= (:score eval) 0))

(defn population [word]
  (let [letter# (count word)
        start (babel letter#)]
    (iterate (fn [_] (babel letter#)) start)))