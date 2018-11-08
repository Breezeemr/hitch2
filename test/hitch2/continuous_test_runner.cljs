(ns hitch2.continuous-test-runner
  (:require [cljs.test]
            [cljs-test-display.core]
            [hitch2.graph-manager.atom-tests]
            [hitch2.graph-test]
            [hitch2.curator.http-test])
  (:require-macros [cljs.test]))

(defn test-run []
  (cljs.test/run-tests
   (cljs-test-display.core/init! "app")
   'hitch2.graph-manager.atom-tests
   'hitch2.graph-test
   'hitch2.curator.http-test))

(test-run)
