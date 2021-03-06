(ns periculum.examples.tic-tac-toe
  (:require [periculum.dsl :refer [deflearn]]
            [periculum.rl :as rl]
            [periculum.more :refer [find-some]]
            [clojure.core.async :as async]
            [clojure.string :as s]))

(def outcomes {:X :O :draw :continue})

(def new-game
  {:board [[nil nil nil]
           [nil nil nil]
           [nil nil nil]]
   :turn   :X})

(defn opponent [player]
  (case player
    :X :O
    :O :X))

(defn player? [elem]
  (or (= :X elem) (= :O elem)))

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

(defn transition [{:keys [board turn] :as a} [x y]]
  {:board (assoc-in board [y x] turn)
   :turn  (opponent turn)})

(defn reward [player]
  (fn [state action]
    (case (->> action (transition state) (:board) (outcome))
      :X        (if (= player :X) 1 -1)
      :O        (if (= player :O) 1 -1)
      :draw     0
      :continue 0
      0)))

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
  (contains? #{:X :O :draw} (outcome board)))

(defn- ptic-tac-toe [player episodes]
  (letfn [(run [f] (async/<!! (f)))]
    (run
      (deflearn {:action     actions
                 :reward     (reward player)
                 :transition transition
                 :terminal   terminal?
                 :start     new-game
                 :algorithm  rl/sarsa-max
                 :policy     (rl/eps-greedy 0.7)
                 :alpha      0.2
                 :gamma      1.0
                 :episodes   episodes}))))

(defn tic-tac-toe [episodes]
  (let [half  (/ episodes 2)
        X-exp (ptic-tac-toe :X half)
        O-exp (ptic-tac-toe :O half)]
    (->> (:q-values O-exp)
         (merge-with merge (:q-values X-exp))
         (assoc X-exp :q-values))))

;; --------------- TIC TAC TOE terminal game ---------------

(defn show-cell [player]
  (case player
    :X "X"
    :O "O"
    " "))

(defn show-board [board]
  (let [cell (fn [row n] (show-cell (row n)))
        row #(str "| " (cell % 0) " | " (cell % 1) " | " (cell % 2) " |")]
    (->> board (map row) (s/join "\n"))))

(defn show-game [{:keys [board turn]}]
  (str "\n"
       "-------------"
       "\n"
       (show-board board)
       "\n"
       "-------------"
       "\n\n"
       "-> " (show-cell turn) "'s turn"))

(defn parse-move [string-move]
  (try
    (let [cell (->> #" "
                    (s/split string-move)
                    (mapv #(Integer/parseInt %)))]
      (when (= 2 (count cell)) cell))
    (catch Exception _ nil)))

(defn translate [string-move board]
  (when-let [[x y] (parse-move string-move)]
    (when (and (<= 0 x 2)
               (<= 0 y 2)
               (nil? (get-in board [y x] nil))) [x y])))

(defn read-move [board]
  (-> (read-line) (translate board) (or (read-move board))))

(defn results [game]
  (case (outcome (:board game))
    :X    "Player X wins"
    :O    "Player O wins"
    :draw "It's a draw"
    "I don't know this"))

(defn play [rl-opponent player]
  "Allows you to play a game of tic-tac-toe against a trained opponent.

  Takes an `opponent` (the learned data returned by any of the RL algorithm from the rl namespace)
  and a `player` (either :X or :O, indicating which one you would like to play)
  and runs a game of tic-tac-toe.

  Every turn, you'll be required to input a position where to place your token.
  The position format is two integers separated by a space, e.g. 0 0 or 1 0.

  To get an opponent, train some experience by running `tic-tac-toe`.

  Example: (play (tic-tac-toe 10000) :X) "
  (let [choose-best   (rl/best-move-max rl-opponent actions)
        player-turn? #(= player (:turn %))]
    (loop [current-game new-game]
      (println (show-game current-game))
      (Thread/sleep 500)
      (cond
        (terminal? current-game)     (println (results current-game))
        (player-turn?  current-game) (->> current-game (:board) (read-move) (transition current-game) (recur))
        :otherwise                   (->> (choose-best current-game) (transition current-game) (recur))))))