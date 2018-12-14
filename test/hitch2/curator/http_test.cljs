(ns hitch2.curator.http-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            [hitch2.curator.http :as http]
            [hitch2.graph :as graph]
            [hitch2.curator.mutable-var :refer  [mutable-var]]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.selector-impl-registry :as reg
             :refer [registry-resolver]]))

(def gctors
  [["Atom graph: " (fn [] (atom-gm/make-gm registry-resolver))]])

(doseq [[graph-name gctor] gctors]
  (deftest simple-get-ok
    (let [graph (gctor)]
      (async done
        (graph/hook-sel graph (fn [[status value :as result]]
                            (is (= result [:ok "cat\n"]) graph-name)
                            (done))
          (http/http "/test.txt" :get nil nil nil nil nil)))))

  (deftest simple-get-error
    (let [graph (gctor)]
      (async done
        (graph/hook-sel graph (fn [[status error :as result]]
                            (is (= status :error) graph-name)
                            (done))
          (http/http "/DOES-NOT-EXIST" :get nil nil nil nil nil)))))

  (deftest simple-refresh
    (testing "Additional http request is issued after a ::http/refresh command. (Must verify manually in dev console.)"
      (let [graph (gctor)
            sel   (http/http "/test.txt" :get nil nil nil nil nil)]
        (async done
          (graph/hook-sel graph
            (fn [result]
              (is (= result [:ok "cat\n"]) (str graph-name " value before refresh"))

              ;; Second hook is to ensure the selector has not been destroyed
              ;; so the second http request is not elided somehow.
              (graph/hook-sel graph
                (fn [result]
                  (is (= result [:ok "cat\n"]) (str graph-name " value after refresh, next frame"))
                  (done))
                sel)
              (graph/apply-commands graph [[sel [::http/refresh sel]]])
              (is (= result [:ok "cat\n"]) (str graph-name " value after refresh")))
            sel))))))
