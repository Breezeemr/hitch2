(ns hitch2.graph-manager.atom-tests
  (:require
    [hitch2.graph :as hitch]
    [hitch2.machine.mutable-var :as mv]
    [hitch2.graph-manager.atom :as g]
            [hitch2.protocols.graph-manager :as gm-proto]
            [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.selector :as sel-proto]
            #?(:cljs [cljs.test :refer [deftest is testing]]
               :clj [clojure.test :refer [deftest is testing]])))

(def initial-node (assoc machine-proto/initial-node :state {}))

(defn no-op-machine [state]
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [_machine] :hitch.selector.kind/machine)
    machine-proto/Init
    (-initialize [_machine] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ g-v node children parents parent-selectors]
      (swap! state update :parent-changes (fnil conj #{}) parent-selectors))
    machine-proto/Commandable
    (-apply-command [_ graph-value node children parents command]
      )))

(deftest atom-tests
  (let [graph-manager (g/make-gm)]
    ;needs to be async
    (hitch/hook-sel graph-manager (fn [gm val] (prn "yay! I got value " val)) (mv/v-sel :test-name))
    (gm-proto/-transact! graph-manager (mv/->mutable-machine :test-name) [:set-value 5])

    ;; what goes here?
    #_(gm-proto/-transact! graph-manager hook/hook-machine
                         [:hook-subscribe ])))
