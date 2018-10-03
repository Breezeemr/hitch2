(ns hitch2.machine.http-test
  (:require [cljs.test :refer [deftest is testing async]]
            [hitch2.machine.http :as http]
            [hitch2.graph :as graph]
            [hitch2.machine.mutable-var :refer  [mutable-var]]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.selector-impl-registry :as reg
             :refer [registry-resolver]]))

(def url "http://dummy.restapiexample.com/api/v1/employees")
(defn deserializer [payload]
  (let [json (.parse js/JSON payload)]
    (first json)))

(def http-call
  (http/http "http://dummy.restapiexample.com/api/v1/employees"
             :get
             nil
             deserializer
             nil nil nil))

(defn assert-valid-http-call [done graph-name [status value :as result]]
  (let [decoded (js->clj value)]
    (is (and (map? decoded)
             (seq decoded))
        graph-name))
  (done))

(def gctors
  [["Atom graph: " (fn [] (atom-gm/make-gm registry-resolver))]])

(doseq [[graph-name gctor] gctors]
  (deftest simple-get-ok
    (binding [atom-gm/*trace* true]
      ;;(atom-gm/clear-trace!)
      (let [graph (gctor)]
        (async done
               (graph/hook-sel graph
                               (partial assert-valid-http-call done graph-name)
                               http-call)))))

  (deftest simple-get-error
    (let [graph (gctor)]
      (async done
        (graph/hook graph (fn [[status error :as result]]
                            (is (= status :error) graph-name)
                            (done))
          http/http "/DOES-NOT-EXIST" :get nil nil nil nil nil))))

  (deftest simple-refresh
    (testing "Additional http request is issued after a ::http/refresh command. (Must verify manually in dev console.)"
      (let [graph (gctor)]
        (async done
               (graph/hook-sel graph
                               (partial assert-valid-http-call done graph-name)
                               
                               http-call))))))
