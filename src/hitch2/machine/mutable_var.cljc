(ns hitch2.machine.mutable-var
  (:require [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto]))


(def initial-node (assoc machine-proto/initial-node :state {}))

(def impl (reify
            sel-proto/ImplementationKind
            (-imp-kind [machine] :hitch.selector.kind/machine)))

(def mutable-machine
  (reify
    sel-proto/SelectorImplementation
    (-imp [machine-instance] impl)
    machine-proto/Init
    (-initialize [machine-instance] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ graph-value node children parents parent-selectors]
      )
    machine-proto/Commandable
    (-apply-command [_ graph-value node children parents command]
      (case (nth command 0)
        ))))


#_(defmethod graph-proto/run-effect :notify [effect]
  (prn effect))

