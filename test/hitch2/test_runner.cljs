;; This test runner is intended to be run from the command line
(ns hitch2.test-runner
  (:require
    ;; require all the namespaces that you want to test
    [cljs.test :refer [report]]
    [figwheel.main.testing :refer-macros [run-tests-async]]
    [figwheel.main.async-result :as async-result]
    [hitch2.graph-manager.atom-tests]
    [hitch2.graph-test]
    [hitch2.curator.http-test]
    [hitch2.descriptor-maps.robin-hood-test]
    ))


(defmethod report [:cljs.test/default :end-run-tests] [test-data]
  (if (cljs.test/successful? test-data)
    (async-result/send "Tests passed!!")
    (async-result/send "Tests failed!!")
    ;(async-result/throw-ex (ex-info "Tests Failed" test-data))
    ))

(defn -main [& args]
  (run-tests-async 50000
    'hitch2.graph-test
    'hitch2.curator.http-test
    'hitch2.graph-manager.atom-tests
    'hitch2.descriptor-maps.robin-hood-test
    ))
