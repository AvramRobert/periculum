(ns meetup.world
  (:require [periculum.domain :as d]
            [periculum.play :refer [world start result-channel]]
            [periculum.dsl :refer [deflearn]]
            [clojure.core.async :as async]
            [periculum.rl :as rl]))

(def start-state start)

(defn terminal? [state]
  ((d/terminal? world) state))

(defn transition [state action]
  ((d/transition world terminal?) state action))

(defn reward [state action]
  ((d/reward world terminal?) state action))

(defn actions [S] (d/actions S))