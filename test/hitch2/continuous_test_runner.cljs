(ns hitch2.continuous-test-runner
  (:require [cljs.test]
            [cljs-test-display.core]
            [hitch2.graph-manager.atom-test]
            [hitch2.graph-manager.mock-test]
            [hitch2.defdescriptor-test]
            [hitch2.graph-test]
            [hitch2.curator.http-test])
  (:require-macros [cljs.test]))

(defn test-run []
  (cljs.test/run-tests
   (cljs-test-display.core/init! "app")
   'hitch2.defdescriptor-test
   'hitch2.graph-manager.atom-test
   'hitch2.graph-test
   'hitch2.curator.http-test
    'hitch2.graph-manager.mock-test))

(test-run)
