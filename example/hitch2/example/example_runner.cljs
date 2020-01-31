(ns hitch2.example.example-runner
  (:require [crinkle.component :refer [RE CE] :as c]
            [crinkle.dom :as d]
            ["react-dom" :refer [render]]
            ))

(defn app [{:keys []}]
  (d/div {} "fine, hellow"))

(defn -main [& args]
  (render
   (CE app {})
   (.. js/document (getElementById "app"))))
