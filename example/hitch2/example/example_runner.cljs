(ns ^:figwheel-hooks hitch2.example.example-runner)

(defn ^:after-load -main [& args]
  (.log js/console "derna lorne"))

;; first-load hack
(-main)
