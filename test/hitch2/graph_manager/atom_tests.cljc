(ns hitch2.graph-manager.atom-tests
  (:require
    [hitch2.graph :as hitch]
    [hitch2.curator.mutable-var :as mv]
    [hitch2.graph-manager.atom :as g]
    [hitch2.protocols.graph-manager :as gm-proto]
    [hitch2.protocols.curator :as machine-proto]
    [hitch2.protocols.selector :as sel-proto]
    [hitch2.selector-impl-registry :as reg
     :refer [registry-resolver]]
    #?(:cljs [cljs.test :refer-macros [deftest is testing]]
       :clj  [clojure.test :refer [deftest is testing]])))

(def initial-node (assoc machine-proto/initial-curator-state :state {}))

(defn no-op-machine [state]
  {:hitch.selector.impl/kind :hitch.selector.kind/machine
   ::machine-proto/init (fn [machine-selector] initial-node)
   ::machine-proto/observed-value-changes
                             (fn [machine-selector graph-value node parent-selectors]
                               (swap! state update :parent-changes (fnil conj #{}) parent-selectors))
   ::machine-proto/apply-command
                             (fn [machine-selector graph-value node command]
                               node)})

(deftest atom-tests
  (let [graph-manager (g/make-gm registry-resolver)
        test-atom (atom nil)]
    ;needs to be async
    (prn "hook")
    (hitch/hook-sel graph-manager (fn [val]
                                    (reset! test-atom val)
                                    (prn "yay! I got value " val)) (mv/mutable-var :test-name))

    (prn "transact" )
    (gm-proto/-transact! graph-manager (mv/mutable-machine :test-name) [:set-value 5])
    (is (= @test-atom 5))
    ;; what goes here?
    #_(gm-proto/-transact! graph-manager hook/hook-machine
                         [:hook-subscribe ])))
