(ns hitch2.graph-manager.mock-test
  (:require
    #?(:cljs [cljs.pprint :as pprint]
       :clj [clojure.pprint :as pprint])
    [hitch2.graph :as hitch]
    [hitch2.curator.mutable-var :as mv]
    [hitch2.graph-manager.atom :as g]
    [hitch2.protocols.graph-manager :as gm-proto]
    [hitch2.def.curator :as curator-proto]
    [hitch2.descriptor :as descriptor]
    [hitch2.def.halting :refer [defhalting]]
    [hitch2.descriptor-impl-registry :as reg
     :refer [registry-resolver]]
    [hitch2.test-common :as common]
    [hitch2.mock :refer [mock-registry-resolver]]
    [hitch2.graph-manager.debug :as debug]
    #?(:cljs [cljs.test :refer-macros [deftest is testing async]]
       :clj  [clojure.test :refer [deftest is testing]])))

(declare fibb-graph)

(defhalting fibb-graph [G n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (let [n-1 (hitch/select! G fibb-graph (dec n))
              n-2 (hitch/select! G fibb-graph (dec (dec n)))]
          (+ @n-1 @n-2))))


(deftest mock-instrument-tests
  (let [graph-manager (g/make-gm
                        (mock-registry-resolver
                          #{'hitch2.curator.pin/pin-curator-spec}
                          #{})
                        #_common/sync-scheduler)
        fibber (fn [n] (descriptor/positional-dtor  fibb-graph n))]
                                        ;needs to be async
    (hitch/pin graph-manager (fibber 30))
    (is (= #{(fibber 29) (fibber 28)}
           (gm-proto/-observes graph-manager (fibber 30))))))

(deftest mock-redepend-on-descriptor-bug
  (let [graph-manager (g/make-gm
                        (mock-registry-resolver
                          #{'hitch2.curator.pin/pin-curator-spec
                            'hitch2.curator.hook/hook-curator-spec}
                          #{})
                        #_common/sync-scheduler)
        fibber (fn [n] (descriptor/positional-dtor  fibb-graph n))
        fib-dtor   (fibber 2)]
                                        ;needs to be async
    (hitch/pin graph-manager fib-dtor)
    (hitch/unpin graph-manager fib-dtor)
    #?(:cljs (async done
                    (let [failure (js/setTimeout (fn [] (is false "test timeout") (done)) 500)]
                      (hitch/hook-sel graph-manager
                                      (fn [fib-result]
                                        (js/clearTimeout failure)
                                        (is (= fib-result 8))
                                        (done))
                                      (fibber 6))))
       :clj (let [result (promise )]
              (hitch/hook-sel graph-manager
                              (fn [fib-result]
                                (deliver result fib-result))
                fib-dtor)
              (is (= 1 (deref result 500 :failure)))))))

