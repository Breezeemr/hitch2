(ns hitch2.graph-manager.atom-tests
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
    [hitch2.graph-manager.debug :as debug]
    #?(:cljs [cljs.test :refer-macros [deftest is testing async]]
       :clj  [clojure.test :refer [deftest is testing]])))

(def initial-node (assoc curator-proto/initial-curator-state :state {}))

(defn no-op-curator [state]
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/curator
   ::curator-proto/init (fn [curator-descriptor] initial-node)
   ::curator-proto/observed-value-changes
                             (fn [curator-descriptor graph-value node parent-descriptors]
                               (swap! state update :parent-changes (fnil conj #{}) parent-descriptors))
   ::curator-proto/apply-command
                             (fn [curator-descriptor graph-value node command]
                               node)})

(deftest atom-tests
  (let [graph-manager (g/make-gm registry-resolver common/sync-scheduler)
        test-atom (atom nil)]
    ;needs to be async
    (hitch/hook-sel graph-manager (fn [val]
                                    (reset! test-atom val))
                    (mv/mutable-var :test-name))

    (gm-proto/-transact! graph-manager (mv/mutable-curator :test-name) [:set-value 5])
    (is (= @test-atom 5))
    ;; what goes here?
    #_(gm-proto/-transact! graph-manager hook/hook-curator
                           [:hook-subscribe ])))

(declare fibb-graph)

(defhalting fibb-graph [G n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (let [n-1 (hitch/select! G fibb-graph (dec n))
              n-2 (hitch/select! G fibb-graph (dec (dec n)))]
          (+ @n-1 @n-2))))


(declare depends-on)
(defhalting depends-on [G n]
  (cond (= 0 n) @(hitch/select-sel! G (mv/mutable-var :bench))
        :else   (+ 1 @(hitch/select! G depends-on (dec n)))))

(deftest instrument-tests
  (let [graph-manager (g/make-gm registry-resolver common/sync-scheduler)
        test-atom (atom nil)
        fibber (fn [n] (descriptor/positional-dtor  fibb-graph n))]
                                        ;needs to be async
    (hitch/pin graph-manager (fibber 30))
    (is (= #{(fibber 29) (fibber 28)}
           (gm-proto/-observes graph-manager (fibber 30))))))

(deftest redepend-on-descriptor-bug
  (let [graph-manager (g/make-gm registry-resolver common/sync-scheduler)
        test-atom (atom nil)
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


(deftest depend-on-descriptor
  (let [graph-manager (g/make-gm registry-resolver common/sync-scheduler)
        test-atom (atom nil)
        depends-on (fn [n] (descriptor/positional-dtor  depends-on n))
        depends-on-dtor   (depends-on 2)]
    ;needs to be async
    (hitch/pin graph-manager depends-on-dtor)
    (gm-proto/-transact! graph-manager (mv/mutable-curator :bench) [:set-value 5])
    #?(:cljs (async done
               (let [failure (js/setTimeout (fn [] (is false "test timeout") (done)) 500)]
                 (hitch/hook-sel graph-manager
                   (fn [fib-result]
                     (js/clearTimeout failure)
                     (is (= fib-result 7))
                     (done))
                   depends-on-dtor)))
       :clj (let [result (promise )]
              (hitch/hook-sel graph-manager
                (fn [fib-result]
                  (deliver result fib-result))
                depends-on-dtor)
              (is (= 1 (deref result 500 :failure)))))))
