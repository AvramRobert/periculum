(ns periculum.learning-spec
  (:use [periculum.world])
  (:use [periculum.learning])
  (:use [periculum.more])
  (:use [clojure.test])
  (:require
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :refer [defspec]]))

(def test-h-max 4)
(def test-t-apex 2)
(def test-G (gravity test-h-max test-t-apex))
(def test-rewards {:tic   -1
                   :solid -5
                   :end   20
                   })

(def test-actions {:stand      (->Action 0 1 [0 0])
                   :walk-left  (->Action 1 1 [-1 0])
                   :walk-right (->Action 1 1 [1 0])
                   :run-left   (->Action 2 1 [-1 0])
                   :run-right  (->Action 2 1 [1 0])
                   :jump       (->Action (Math/round ^Float (jump-velocity test-G test-h-max)) test-t-apex [0 1])
                   :fall       (->Action test-G 1 [0 -1])
                   })

(defn is-found? [coll]
  (let [item (rand-nth coll)]
    (= (find-some #(= % item) coll) item)))

(defn by-eta? [latt]
  (let [world (solidify-many latt)
        item (rand-nth world)
        state (->State (:position item) :stand)
        lookup (eta-one world {})]
    (= (lookup state) item)))

(defn by-eta-sec? [latt]
  (let [world (solidify-many latt)
        item (rand-nth world)
        items (filter #(= item %) world)
        state (->State (:position item) :stand)
        lookup (eta world {})]
    (= (lookup state) items)))

(defspec find-first
         100
         (prop/for-all [v (gen/such-that #(not-empty %) (gen/vector gen/int))]
                       (is-found? v)))

(def pos-gen
  (gen/fmap (fn [[x y]]
              (pos x y)) (gen/tuple gen/pos-int gen/pos-int)))

(defspec eta-lookup
         100
         (prop/for-all [ps (gen/such-that #(not-empty %) pos-gen)]
                       (by-eta? ps)))

(defspec eta-sec-lookup
         100
         (prop/for-all [ps (gen/such-that #(not-empty %) pos-gen)]
                       (by-eta-sec? ps)))

(def world-config {:floor     (m-struct 15 0 (pos 0 0))
                   :holes     [(pos 3 0) (pos 13 0) (pos 14 0) (pos 15 0)]
                   :walls     [(m-struct 2 3 (pos 5 1)) (m-struct 2 3 (pos 10 1))]
                   :platforms empty-vec
                   })

(def world (make-world world-config))
(def lookup (eta-pos world test-actions))
(def movement (move lookup))
(def jumping (jump lookup))
(def Ω (omega world test-actions))
(def rewardf (reward world
                     test-actions
                     test-rewards
                     #(= (:position %) (pos 12 1))))
(def transitionf (transition world
                             test-actions
                             #(= (:position %) (pos 12 1))))


(deftest jump-properly
  (let [start1 (pos 0 1)
        [t-js path-js] (jumping start1 :stand)              ; jump standing
        expected-js [(pos 0 1) (pos 0 2) (pos 0 3) (pos 0 4) (pos 0 5) (pos 0 5) (pos 0 4) (pos 0 3) (pos 0 2) (pos 0 1)]
        start2 (pos 3 1)
        [t-jwr path-jwr] (jumping start2 :walk-right)       ; jump walking right
        expected-jwr [(pos 3 1) (pos 4 2) (pos 4 3) (pos 5 4) (pos 5 5) (pos 5 5) (pos 6 4)]
        start3 (pos 3 1)
        [t-jwl path-jwl] (jumping start3 :walk-left)        ; jump walking left
        expected-jwl [(pos 3 1) (pos 3 2) (pos 3 3) (pos 2 3) (pos 2 4) (pos 1 5) (pos 1 5) (pos 1 4) (pos 0 3) (pos 0 2) (pos 0 1)]
        ]
    (is (and
          (= path-js expected-js)
          (= t-js 4)
          ))
    (is (and
          (= path-jwr expected-jwr)
          (= t-jwr 3)
          )
        (and
          (= path-jwl expected-jwl)
          (= t-jwl 5)))))

(deftest move-properly
  (let [start (pos 0 1)
        [t-wr path-wr] (movement start :walk-right)         ; walk right
        expected-w-right [(pos 0 1) (pos 1 1)]
        [t-wl path-wl] (movement (last path-wr) :walk-left) ; walk left
        expected-w-left [(pos 1 1) (pos 0 1)]
        [t-rr path-rr] (movement start :run-right)          ; run right
        expected-r-right [(pos 0 1) (pos 1 1) (pos 2 1)]
        [t-rl path-rl] (movement (last path-rr) :run-left)  ; run left
        expected-r-left [(pos 2 1) (pos 1 1) (pos 0 1)]
        [t-s path-s] (movement start :stand)                ; stand
        expected-s [(pos 0 1)]
        start2 (pos 6 4)
        [t-fwr path-fwr] (movement start2 :walk-right)      ; walk right and fall
        expected-f-right [(pos 6 4) (pos 7 4) (pos 7 3) (pos 7 2) (pos 7 1)]
        ]
    (is (and
          (= path-wr expected-w-right)
          (= t-wr 1)))
    (is (and
          (= path-wl expected-w-left)
          (= t-wl 1)))
    (is (and
          (= path-rr expected-r-right)
          (= t-rr 1)))
    (is (and
          (= path-rl expected-r-left)
          (= t-rl 1)))
    (is (and
          (= path-s expected-s)
          (= t-s 1)))
    (is (and
          (= path-fwr expected-f-right)
          (= t-fwr 2)))))

(deftest fall-properly
  (let [start1 (pos 7 4)
        [t-s-desc path-ds] (fall start1 lookup [:stand])    ; fall standing
        expected-ds [(pos 7 4) (pos 7 3) (pos 7 2) (pos 7 1)]
        start2 (pos 6 4)
        [t-wr-desc path-dwr] (fall start2 lookup [:walk-right]) ; fall walking right
        expected-dwr [(pos 6 4) (pos 7 3) (pos 7 2) (pos 8 1)]
        [t-rr-desc path-drr] (fall start2 lookup [:run-right]) ; fall running right
        expected-drr [(pos 6 4) (pos 7 3) (pos 8 2) (pos 9 1)]
        start3 (pos 1 4)
        [t-rl-desc path-drl] (fall start3 lookup [:run-left])
        expected-drl [(pos 1 4) (pos 0 3) (pos 0 3) (pos 0 2) (pos 0 1)]
        ]
    (is (and
          (= path-ds expected-ds)
          (= t-s-desc 2)
          ))
    (is (and
          (= path-dwr expected-dwr)
          (= t-wr-desc 2)
          ))
    (is (and
          (= path-drr expected-drr)
          (= t-rr-desc 2)
          ))
    (is (and
          (= path-drl expected-drl)
          (= t-rl-desc 2)
          ))))

(deftest omega-test
  (let [state1 (->State (pos 0 1) :walk-right)
        [t-rr path-rr] (Ω state1 :run-right)
        expected-rr [(pos 0 1) (pos 1 1) (pos 2 1)]
        state2 (->State (pos 1 1) :run-right)
        [t-s path-s] (Ω state2 :stand)
        expected-s [(pos 1 1)]
        state3 (->State (pos 3 1) :walk-right)
        [t-jwr path-jwr] (Ω state3 :jump-right)
        expected-jwr [(pos 3 1) (pos 4 2) (pos 4 3) (pos 5 4) (pos 5 5) (pos 5 5) (pos 6 4)]]
    (is (and
          (= (map #(:position %) path-rr) expected-rr)
          (= (-> path-rr first :previous-action) :run-right)
          (= t-rr 1)
          ))
    (is (and
          (= (map #(:position %) path-s) expected-s)
          (= (-> path-s first :previous-action) :stand)
          (= t-s 1)))
    (is (and
          (= (map #(:position %) path-jwr) expected-jwr)
          (= (-> path-jwr first :previous-action) :jump-right)
          (= t-jwr 3)))))

(deftest reward-test
  (let [state1 (->State (pos 1 1) :stand)
        res-walk-right (rewardf state1 :walk-right)
        state2 (->State (pos 2 1) :stand)
        res-walk-right-fall (rewardf state2 :walk-right)
        state3 (->State (pos 6 4) :stand)
        res-walk-right-fall-longer (rewardf state3 :walk-right)
        state4 (->State (pos 11 4) :stand)
        res-walk-fall-win (rewardf state4 :walk-right)
        state5 (->State (pos 4 1) :walk-right)
        res-jump-hit-wall (rewardf state5 :jump-right)
        state6 (->State (pos 0 1) :stand)
        res-run (rewardf state6 :run-right)]
    (is (= -1 res-walk-right))
    (is (= -6 res-walk-right-fall))
    (is (= -2 res-walk-right-fall-longer))
    (is (= 18 res-walk-fall-win))
    (is (= -14 res-jump-hit-wall))
    (is (= -1 res-run))
    ))

(deftest transition-test
  (let [state1 (->State (pos 1 1) :stand)
        res-walk-right (transitionf state1 :walk-right)
        state2 (->State (pos 2 1) :stand)
        res-walk-right-fall (transitionf state2 :walk-right)
        state3 (->State (pos 10 4) :stand)
        res-run-fall-win (transitionf state3 :run-right)
        ]
    (is (= res-walk-right (->State (pos 2 1) :walk-right)))
    (is (= res-walk-right-fall (->State (pos 3 0) :walk-right)))
    (is (= res-run-fall-win (->State (pos 12 1) :run-right)))
    ))

(deftest action-test
  (let [act-run-left (actions (->State nil :run-left))
        expected-run-let [:stand
                          :walk-left
                          :walk-right
                          :run-left
                          :run-right
                          :run-jump-left]
        act-jump-up (actions (->State nil :jump-up))
        expected-jump-up  [:stand
                           :walk-left
                           :walk-right
                           :jump-up
                           :run-left
                           :run-right
                           :jump-left
                           :jump-right]]
    (is (= act-run-left expected-run-let)
        (= act-jump-up expected-jump-up))))

(run-tests)