(ns periculum.rl-spec
  (:use [periculum.rl])
  (:use [clojure.test])
  (:use [periculum.more])
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
  (let [policy (fn [S _ _ _]
                 (if (even? (:x S))
                   :left
                   :right))
        action-f identity
        reward-f (fn [_ _] -1)
        transition-f (fn [S _] (->Pos (dec (:x S)) (:y S)))
        start (->Pos 0 0)
        gen (lazy-traj policy action-f reward-f transition-f)
        res (take 10 (gen start {} 0))
        expected (for [n (range 0 -9)]
                   (if (even? n)
                     (->Sample (->Pos n 0) :left -1)
                     (->Sample (->Pos n 0) :left -1)))]
    (is res expected)))

(deftest Q-test
  (let [data {:q-values {(->Pos 0 0) {:left  1
                                      :right 1}
                         (->Pos 0 1) {:left  2
                                      :right 2}
                         (->Pos 0 2) {:left  3
                                      :right 2}}}]
    (is (= (Q data (->Pos 0 0) :left) 1))
    (is (= (Q data (->Pos 0 2) :right) 2))
    (is (= (Q data (->Pos 1 1) :left) 0))))

(deftest C-test
  (let [data {:counts {(->Pos 0 0) {:left  1
                                    :right 2}
                       (->Pos 0 1) {:left 2}
                       (->Pos 0 2) {:right 3}
                       }}]
    (is (= (C data (->Pos 0 0) :left) 1))
    (is (= (C data (->Pos 0 1) :right) 0))
    (is (= (C data (->Pos 1 1) :right) 0))))

(defn grouped-map [values init f]
  (map (fn [[ks vs]]
         (let [value (get-in values [(first ks) (second ks)])]
           (f value vs))) (group-by (fn [v]
                                      [(:state v) (:action v)]) init)))

(defspec chain-test
         100
         (prop/for-all [v (gen/such-that not-empty (gen/vector gen-sample))]
                       (let [data {:q-values {}}
                             ndata (from-chain data v (fn [old cur]
                                                        (+ old (:reward cur))))
                             summed-eq (grouped-map (:q-values ndata) v
                                                    (fn [cmptd cur]
                                                      (= cmptd (reduce #(+ %1 (:reward %2)) 0 cur))))]
                         (is (every? #(contains? (:q-values ndata) (:state %)) v))
                         (is (every? identity summed-eq)))))


(defspec every-visit-inc-tst
         100
         (prop/for-all [v (gen/such-that not-empty (gen/vector gen-sample))]
                       (let [data {:counts {}}
                             ndata (every-visit-inc data v)
                             summed-eq (grouped-map (:counts ndata) v
                                                    (fn [cmptd grpd]
                                                      (= cmptd (count grpd))))]
                         (is (every? identity summed-eq)))))

(defn mean [coll]
  (/ (reduce + coll) (count coll)))

(defspec mc-update-test
         100
         (prop/for-all [v (gen/such-that not-empty (gen/vector gen/int))
                        +elm gen/int]
                       (let [init-mean (mean v)
                             inc-mean (mc-update init-mean +elm (+ 1 (count v)))
                             exp-mean (mean (conj v +elm))]
                         (is (= inc-mean exp-mean)))))


(deftest Q-sarsa-λ-test
  (let [data1 {:alpha 1
              :counts {(->Pos 0 0) {:left 1 :right 2}
                       (->Pos 0 1) {:right 1}
                       (->Pos 0 2) {:left 1 :right 2}}}
        As {:left 1 :right 2}
        res1 (Q-sarsa-λ data1 (->Pos 0 0) As 1)
        res2 (Q-sarsa-λ data1 (->Pos 0 2) As 2)
        data2 (assoc data1 :alpha 4)
        res3 (Q-sarsa-λ data2 (->Pos 0 1) As 2)
        res4 (Q-sarsa-λ data2 (->Pos 0 0) As 2)]
    (is (= {:left 2 :right 4} res1))
    (is (= {:left 3 :right 6} res2))
    (is (= {:left 1 :right 10} res3))
    (is (= {:left 9 :right 18} res4))))

(deftest E-sarsa-λ-test
  (let [data1 {:alpha 1
               :lambda 1
               :counts {(->Pos 0 0) {:left 1 :right 2}
                        (->Pos 0 1) {:right 1}
                        (->Pos 0 2) {:left 1 :right 2}}}
        res1 (E-sarsa-λ data1 (->Pos 0 0))
        res2 (E-sarsa-λ data1 (->Pos 0 1))
        data2 (assoc data1 :alpha 7 :lambda 3)
        res3 (E-sarsa-λ data2 (->Pos 0 2))]
    (is (= {:left 1 :right 2} res1))
    (is (= {:right 1} res2))
    (is (= {:left 21 :right 42} res3))))

(run-tests)