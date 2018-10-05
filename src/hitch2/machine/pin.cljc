(ns hitch2.machine.pin
  (:require [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto
             :refer [def-selector-spec]]
            [hitch2.selector-impl-registry :as reg]))

(def initial-node (assoc machine-proto/initial-machine-state :state #{}))

(def-selector-spec pin-machine-spec
  :hitch.selector.spec.kind/positional-params)

(def pin-machine-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [machine] :hitch.selector.kind/machine)
    machine-proto/Init
    (-initialize [machine-instance machine-selector]
      initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ machine-selector graph-value node children parents parent-selectors]
      node)
    machine-proto/Commandable
    (-apply-command [_ machine-selector graph-value node children parents command]
      (case (nth command 0)
        :pin
        (let [[_ selector] command
              there? (get-in node [:state selector])]
          (if there?
            node
            (-> node
                (update :state conj selector)
                (update :change-parent assoc selector true))))
        :unpin
        (let [[_ selector] command
              there? (get-in node [:state selector])]
          (if there?
            (-> node
                (update :state disj selector)
                (update :change-parent assoc selector false))
            node))))))

(reg/def-registered-selector pin-machine-spec' pin-machine-spec pin-machine-impl)

(def pin-machine
  (sel-proto/sel pin-machine-spec'))

