(ns hitch2.machine.pin
  (:require [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto]))

(defrecord node-state [state change-parent reset-vars
                       async-effects sync-effects])
(def initial-node (assoc machine-proto/initial-node :state #{}))

(def impl (reify
            sel-proto/ImplementationKind
            (-imp-kind [machine] :hitch.selector.kind/machine)))

(def pin-machine
  (reify
    sel-proto/SelectorImplementation
    (-imp [machine-instance] impl)
    machine-proto/Init
    (-initialize [machine-instance] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ graph-value node children parents parent-selectors]
      node)
    machine-proto/Commandable
    (-apply-command [_ graph-value node children parents command]
      (case (nth command 0)
        :pin
        (let [[_ selector ] command
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

