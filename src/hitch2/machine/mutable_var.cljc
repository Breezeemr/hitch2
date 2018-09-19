(ns hitch2.machine.mutable-var
  (:require [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto]))


(def initial-node (assoc machine-proto/initial-node :state {}))

(def machine-impl (reify
            sel-proto/ImplementationKind
            (-imp-kind [machine] :hitch.selector.kind/machine)))

(defrecord mutable-machine [ns]
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
      )))

(def var-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [var]
      :hitch.selector.kind/var-singleton-machine)
    sel-proto/GetMachine
    (-get-machine [var [_ var-name]]
      (->mutable-machine var-name))))

(defn v-sel [var-name]
  (sel-proto/->Selector1 var-impl var-name))

