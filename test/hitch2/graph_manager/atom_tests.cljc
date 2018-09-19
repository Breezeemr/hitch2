(ns hitch2.graph-manager.atom-tests
  (:require [hitch2.graph-manager.atom :as g]
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
  )
