(ns periculum.play
  (:require [clojure.core.async :as async]
            [periculum.dsl :refer [deflearn terminal<-]]
            [periculum.world :refer [world-from-pixmap ->Pos]]
            [periculum.domain :refer [terminal? actions reward transition ->State]]))

(def ^:const local-path "./resources/")

(def result-channel (async/chan))

(def scene0_stage0 (world-from-pixmap (str local-path "scene0_stage0.png")))
(def scene0_stage1 (world-from-pixmap (str local-path "scene0_stage1.png")))
(def scene0_stage2 (world-from-pixmap (str local-path "scene0_stage2.png")))

(def scene1_stage0 (world-from-pixmap (str local-path "scene1_stage0.png")))
(def scene1_stage1 (world-from-pixmap (str local-path "scene1_stage1.png")))
(def scene1_stage2 (world-from-pixmap (str local-path "scene1_stage2.png")))

(def scene2_stage0 (world-from-pixmap (str local-path "scene2_stage0.png")))
(def scene2_stage1 (world-from-pixmap (str local-path "scene2_stage1.png")))
(def scene2_stage2 (world-from-pixmap (str local-path "scene2_stage2.png")))

(def levels
  {:scene0_stage0 {:world scene0_stage0
                   :terminal (terminal? scene0_stage0 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))
                   :start (->State (->Pos 1 1) :stand)}
   :scene0_stage1 {:world scene0_stage1
                   :terminal (terminal? scene0_stage1 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 4))))
                   :start (->State (->Pos 1 1) :stand)}

   :scene0_stage2 {:world scene0_stage2
                   :terminal (terminal? scene0_stage2 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))
                   :start (->State (->Pos 1 1) :stand)}

   :scene1_stage0 {:world scene1_stage0
                   :terminal (terminal? scene1_stage0 (fn [state max] (and
                                                                        (>= (-> state :position :x) (- (-> max :position :x) 3))
                                                                        (<= (-> state :position :y) 5))))
                   :start (->State (->Pos 6 13) :stand)}
   :scene1_stage1 {:world scene1_stage1
                   :terminal (terminal? scene1_stage1 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))
                   :start (->State (->Pos 6 13) :stand)}
   :scene1_stage2 {:world scene1_stage2
                   :terminal (terminal? scene1_stage2 (fn [state max] (>= (-> state :position :x) (- (-> max :position :x) 3))))
                   :start (->State (->Pos 6 13) :stand)}

   :scene2_stage0 {:world scene2_stage0
                   :terminal (terminal? scene2_stage0 (fn [state max] (and
                                                                        (>= (-> state :position :x) (- (-> max :position :x) 3)))))
                   :start (->State (->Pos 4 11) :stand)}
   :scene2_stage1 {:world scene2_stage1
                   :terminal (terminal? scene2_stage1 (fn [state max] (and
                                                                        (>= (-> state :position :x) (- (-> max :position :x) 6))
                                                                        (<= (-> state :position :y) 7))))
                   :start (->State (->Pos 4 15) :stand)}
   :scene2_stage2 {:world scene2_stage2
                   :terminal (terminal? scene2_stage2 (fn [state max] (and
                                                                        (>= (-> state :position :x) (- (-> max :position :x) 3))
                                                                        (<= (-> state :position :y) 7))))
                   :start (->State (->Pos 4 24) :stand)}})

(def level :scene0_stage2)
(def world (-> levels (get level) (:world)))
(def start (-> levels (get level) (:start)))

(defn learn [level-key config]
  "Delegates to `deflearn` but presets the primitives to those of the platformer MDP.

   Given a level keyword (from the list of levels) and the standard RL config,
   will return a function that, when run, will run the RL algorithm on the level
   and return a map containing the learned q-values"
  (let [nconf    (merge config (levels level-key {}))
        terminal (terminal<- nconf)
        world    (:world nconf)]
    (-> nconf
        (assoc :action actions)
        (assoc :reward (reward world terminal))
        (assoc :transition (transition world terminal))
        (deflearn))))