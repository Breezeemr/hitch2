;; This test runner is intended to be run from the command line
(ns hitch2.test-runner
  (:require
    ;; require all the namespaces that you want to test
    [figwheel.main.testing :refer [run-tests-async]]
    [hitch2.graph-manager.atom-tests]
    [hitch2.graph-test]
    [hitch2.machine.http-test]
    ))

(defn -main [& args]
  (run-tests-async 5000))
