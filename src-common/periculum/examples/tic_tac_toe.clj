(ns periculum.examples.tic-tac-toe
  (:require [periculum.dsl :refer [deflearn]]
            [periculum.rl :as rl]
            [periculum.more :refer [find-some]]
            [clojure.core.async :as async]))

(def outcomes {:X :O :draw :continue})

(def game
  {:board [[nil nil nil]
           [nil nil nil]
           [nil nil nil]]
   :player :X})

(defn opponent [player]
  (case player
    :X :Y
    :Y :X))

(defn player? [elem]
  (or (= :X elem) (= :Y elem)))

(defn draw? [elem]
  (= :draw elem))

(defn evaluate [board]
  (letfn [(winner [row] (->> row distinct rest empty?))
          (draw   [row] (every? player? row))]
    (cond
      (some winner board) (->> board (find-some winner) (first))
      (every? draw board) :draw
      :otherwise          :continue)))

(defn row-outcome [board]
  (evaluate board))

(defn column-outcome [board]
  (let [[[x1 x2 x3]
         [y1 y2 y3]
         [z1 z2 z3]] board]
    (evaluate [[x1 y1 z1]
               [x2 y2 z2]
               [x3 y3 z3]])))

(defn diagonal-outcome [board]
  (let [[[x1 x2 x3]
         [y1 y2 y3]
         [z1 z2  z3]] board]
    (evaluate [[x1 y2 z3] [x3 y2 z1]])))

(defn outcome [board]
  (let [rows      (row-outcome board)
        columns   (column-outcome board)
        diagonals (diagonal-outcome board)
        outcomes  [rows columns diagonals]]
    (cond
      (some player? outcomes) (find-some player? outcomes)
      (every? draw? outcomes) :draw
      :otherwise              :continue)))

(defn transition [{:keys [board player]} [x y]]
  {:board  (assoc-in board [y x] player)
   :player (opponent player)})

(defn reward [{:keys [player] :as state} action]
  (case (->> action (transition state) (:board) (outcome))
    :X        (if (= player :X) 1 -1)
    :O        (if (= player :O) 1 -1)
    :draw     0
    :continue 0
    0))

(defn actions [{:keys [board]}]
  (or (->> board
           (map-indexed
             (fn [y row]
               (map-indexed (fn [x e] [[x y] e]) row)))
           (mapcat #(filter (comp nil? second) %))
           (map first)
           (seq))
      [:nothing]))

(defn terminal? [{:keys [board]}]
  (contains? #{:X :Y :draw} (outcome board)))

(def tic-tac-toe
  (deflearn {:action     actions
             :reward     reward
             :transition transition
             :terminal   terminal?
             :start      game
             :algorithm  rl/sarsa-max
             :policy     (rl/eps-greedy 0.7)
             :alpha      0.3
             :gamma      1.0
             :episodes   10000}))