(ns hitch2.clojure-runner
  (:require [clojure.test :refer :all]
            [hitch2.hitch2-test]
            [hitch2.graph-manager.atom-tests]
            [hitch2.graph-test]))

(defn -main [& args]
  (run-all-tests))
