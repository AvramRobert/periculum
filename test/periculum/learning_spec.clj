(ns periculum.learning-spec
  (:use [periculum.world])
  (:use [periculum.learning])
  (:use [periculum.coll.more])
  (:use [clojure.test])
  (:require
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :refer [defspec]]))

(defn is-found? [coll]
  (let [item (rand-nth coll)]
    (= (find-some #(= % item) coll) item)))

(defn by-eta? [latt]
  (let [world (solidify-many latt)
        item (rand-nth world)
        state (->State (:position item) :stand)
        lookup (eta world)]
    (= (lookup state) item)))

(defn by-eta-sec? [latt]
  (let [world (solidify-many latt)
        item (rand-nth world)
        items (filter #(= item %) world)
        state (->State (:position item) :stand)
        lookup (eta-sec world)]
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

(def world-config {:floor     15
                   :holes     [(pos 3 0) (pos 4 0) (pos 13 0) (pos 14 0) (pos 15 0)]
                   :walls     [(m-struct 2 3 (pos 5 1)) (m-struct 2 3 (pos 10 1))]
                   :platforms empty-vec
                   })

(def world (make-world world-config))
(def lookup (eta-pos world))
(def movement (move lookup))
(def jumping (jump lookup))

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
          (= t-wr 1)
          (= path-wl expected-w-left)
          (= t-wl 1)
          (= path-rr expected-r-right)
          (= t-rr 1)
          (= path-rl expected-r-left)
          (= t-rl 1)
          (= path-s expected-s)
          (= t-s 1)
          (= path-fwr expected-f-right)
          (= t-fwr 2)
          ))))

(deftest fall-properly
  (let [start1 (pos 7 4)
        [t-s-desc path-ds] (fall start1 lookup [:stand])    ; fall standing
        expected-ds [(pos 7 4) (pos 7 3) (pos 7 2) (pos 7 1)]
        start2 (pos 6 4)
        [t-wr-desc path-dwr] (fall start2 lookup [:walk-right]) ; fall walking right
        expected-dwr [(pos 6 4) (pos 7 3) (pos 7 2) (pos 8 1)]
        [t-rr-desc path-drr] (fall start2 lookup [:run-right]) ; fall running right
        expected-drr [(pos 6 4) (pos 7 3) (pos 8 2) (pos 9 1)]
        ]
    (is (and
          (= path-ds expected-ds)
          (= t-s-desc 2)
          (= path-dwr expected-dwr)
          (= t-wr-desc 2)
          (= path-drr expected-drr)
          (= t-rr-desc 2)
          ))))

(deftest jump-properly
  (let [start1 (pos 0 1)
        [t-js path-js] (jumping start1 :stand)            ; jump standing
        expected-js [(pos 0 1) (pos 0 2) (pos 0 3) (pos 0 4) (pos 0 5) (pos 0 5) (pos 0 4) (pos 0 3) (pos 0 2) (pos 0 1)]
        start2 (pos 3 1)
        [t-jwr path-jwr] (jumping start2 :walk-right)     ; jump walking right
        expected-jwr [(pos 3 1) (pos 4 2) (pos 4 3) (pos 5 4) (pos 5 5) (pos 5 5) (pos 6 4)]
        ]
    (is (and
          (= path-js expected-js)
          (= t-js 5)
          (= path-jwr expected-jwr)
          (= t-jwr 3)
          ))))


(run-tests)