(ns hitch2.clojure-runner
  (:require [clojure.test :refer :all]
            [hitch2.graph-manager.atom-test]
            [hitch2.defdescriptor-test]
            [hitch2.graph-test]))

(defn -main [& args]
  (run-all-tests #"hitch2.*-test"))
