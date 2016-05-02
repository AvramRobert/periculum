(ns periculum.core.desktop-launcher
  (:require [periculum.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. periculum-game "periculum" 800 600)
  (Keyboard/enableRepeatEvents true))
