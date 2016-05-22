(defproject periculum "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  
  :dependencies [[com.badlogicgames.gdx/gdx "1.8.0"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.8.0"]
                 [com.badlogicgames.gdx/gdx-box2d "1.8.0"]
                 [com.badlogicgames.gdx/gdx-box2d-platform "1.8.0"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-bullet "1.8.0"]
                 [com.badlogicgames.gdx/gdx-bullet-platform "1.8.0"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-platform "1.8.0"
                  :classifier "natives-desktop"]
                 [org.clojure/clojure "1.7.0"]
                 [play-clj "1.0.0"]
                 [org.clojure/test.check "0.9.0"]
                 [clj-tuple "0.2.2"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.async "0.2.374"]
                 [handy/map-values "1.0.1"]
                 [net.mikera/imagez "0.10.0"]]
  
  :source-paths ["src" "src-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [periculum.core.desktop-launcher]
  :main periculum.core.desktop-launcher)
