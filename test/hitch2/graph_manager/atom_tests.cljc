(ns hitch2.graph-manager.atom-tests
  (:require
    [hitch2.graph :as hitch]
    [hitch2.curator.mutable-var :as mv]
    [hitch2.graph-manager.atom :as g]
    [hitch2.protocols.graph-manager :as gm-proto]
    [hitch2.def.curator :as machine-proto]
    [hitch2.descriptor :as descriptor]
    [hitch2.def.halting :refer [defhalting]]
    [hitch2.selector-impl-registry :as reg
     :refer [registry-resolver]]
    [hitch2.test-common :as common]
    [hitch2.graph-manager.debug :as debug]
    #?(:cljs [cljs.test :refer-macros [deftest is testing async]]
       :clj  [clojure.test :refer [deftest is testing]])))

(def initial-node (assoc machine-proto/initial-curator-state :state {}))

(defn no-op-machine [state]
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/machine
   ::machine-proto/init (fn [machine-selector] initial-node)
   ::machine-proto/observed-value-changes
                             (fn [machine-selector graph-value node parent-selectors]
                               (swap! state update :parent-changes (fnil conj #{}) parent-selectors))
   ::machine-proto/apply-command
                             (fn [machine-selector graph-value node command]
                               node)})

(deftest atom-tests
  (let [graph-manager (g/make-gm registry-resolver common/sync-scheduler)
        test-atom (atom nil)]
    ;needs to be async
    (hitch/hook-sel graph-manager (fn [val]
                                    (reset! test-atom val))
                    (mv/mutable-var :test-name))

    (gm-proto/-transact! graph-manager (mv/mutable-machine :test-name) [:set-value 5])
    (is (= @test-atom 5))
    ;; what goes here?
    #_(gm-proto/-transact! graph-manager hook/hook-machine
                           [:hook-subscribe ])))

(declare fibb-graph)

(defhalting fibb-graph [G n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (let [n-1 (hitch/select! G fibb-graph (dec n))
              n-2 (hitch/select! G fibb-graph (dec (dec n)))]
          (+ @n-1 @n-2))))

(deftest instrument-tests
  (let [graph-manager (g/make-gm registry-resolver common/sync-scheduler)
        test-atom (atom nil)
        fibber (fn [n] (descriptor/dtor  fibb-graph n))]
                                        ;needs to be async
    (hitch/pin graph-manager (fibber 30))
    (is (= #{(fibber 29) (fibber 28)}
           (gm-proto/-observes graph-manager (fibber 30))))))

(deftest redepend-on-selector-bug
  (let [graph-manager (g/make-gm registry-resolver common/sync-scheduler)
        test-atom (atom nil)
        fibber (fn [n] (descriptor/dtor  fibb-graph n))
        fib-sel   (fibber 2)]
                                        ;needs to be async
    (hitch/pin graph-manager fib-sel)
    (hitch/unpin graph-manager fib-sel)
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
                fib-sel)
              (is (= 1 (deref result 500 :failure)))))))
