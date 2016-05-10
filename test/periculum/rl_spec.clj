(ns periculum.rl-spec
  (:use [periculum.rl])
  (:use [clojure.test])
  (:use [periculum.coll.more])
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def actions [:left :right])
(defrecord Pos [x y])

(def gen-action
  (gen/elements actions))

(def gen-state
  (gen/fmap (fn [idx]
              (keyword (str "state" idx))) gen/pos-int))

(def gen-sample
  (gen/fmap (fn [[S A R]]
              (->Sample S A R))
            (gen/tuple gen-state gen-action (gen/double* {:infinite? false :NaN? false}))))

(defn sum-exp [chain γ]
  (let [discounted (discount chain γ)]
    (assoc (first chain) :reward (reduce + discounted))))


(defspec discounted-reward-test
         100
         (prop/for-all [v (gen/such-that #(not-empty %) (gen/vector gen-sample))
                        d (gen/double* {:infinite? false
                                        :NaN?      false
                                        :min       0.0
                                        :max       1.0})]
                       (let [expected-0 (Gt v 0.0)
                             expected-1 (Gt v d)]
                         (= expected-0 v)
                         (= (first expected-1) (sum-exp v d)))))


(deftest lazy-trajectory-test
  (let [policy (fn [_ _]
                 :left)
        action-f identity
        reward-f (fn [_ _] -1)
        transition-f (fn [S _] (->Pos (dec (:x S)) (:y S)))
        start (->Pos 0 0)
        gen (lazy-traj policy action-f reward-f transition-f)
        res (take 10 (gen start))
        expected (map #(->Sample (->Pos % 0) :left -1) (range 0 -9))]
    (is res expected)))

(run-tests)