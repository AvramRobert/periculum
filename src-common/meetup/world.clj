(ns meetup.world
  (:require [periculum.domain :as d]
            [periculum.play :refer [world start]]
            [periculum.dsl :refer [deflearn]]))

(def start-state start)

(defn terminal? [state]
  (d/terminal? world))

(defn transition [state action]
  (d/transition world d/actions terminal?))

(defn reward [state action]
  (d/reward world terminal?))

(defn actions [S] (d/actions S))