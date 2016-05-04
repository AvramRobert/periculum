(ns periculum.world.world-spec
  (:use [periculum.world])
  (:use [clojure.test])
  (:require
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :refer [defspec]]))

(def expected-distro
  (fn [start]
    [(->Pos (:x start) (:y start)) (->Pos (+ (:x start) 1) (:y start))
     (->Pos (:x start) (+ (:y start) 1)) (->Pos (+ (:x start) 1) (+ (:y start) 1))
     (->Pos (:x start) (+ (:y start) 2)) (->Pos (+ (:x start) 1) (+ (:y start) 2))]))

(defn same-distro? [elms]
  (let [pairs (->> elms (partition 2) (map (fn [[x y]] (pos x y))))]
    (every? #(= ((lattice 2 3) %) (expected-distro %)) pairs)))

(defspec lattice-integrity
   100
  (prop/for-all [samples (gen/such-that
                           #(and
                             (not-empty %)
                             (even? (count %))) (gen/vector gen/pos-int)
                           100)]
       (same-distro? samples)))

(def exp-world-lattice
  [(pos 0 0) (pos 1 0) (pos 2 0) (pos 3 0) ; gap ;
   (pos 6 0) (pos 7 0) (pos 8 0) (pos 9 0) (pos 10 0) ; floor
   (pos 2 1) (pos 3 1) (pos 2 2) (pos 3 2) (pos 2 3) (pos 3 3) ; wall
   (pos 4 5) (pos 5 5) (pos 6 5) ; platform
   ])

(def config {:floor 11
             :holes [(pos 4 0) (pos 5 0)]
             :walls [(m-struct 2 3 (pos 2 1))]
             :platforms [(m-struct 3 (pos 4 5))]})

(deftest world-creation
  (let [world (make-world config)
        exp-world (flatten (solidify-many exp-world-lattice))
        res (every?
              (fn [itm]
                (some #(= itm %) exp-world)) world)]
    (is res)))

(run-tests)
