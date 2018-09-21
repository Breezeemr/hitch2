(ns hitch2.machine.mutable-var
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto]))

(declare v-sel)
(def initial-node (assoc machine-proto/initial-node :state NOT-FOUND-SENTINEL))

(def machine-impl (reify
            sel-proto/ImplementationKind
            (-imp-kind [machine] :hitch.selector.kind/machine)))

(defrecord mutable-machine [ns]
  sel-proto/SelectorImplementation
  (-imp [machine-instance] machine-impl)
  machine-proto/Init
  (-initialize [machine-instance] initial-node)
  machine-proto/Commandable
  (-apply-command [_ graph-value node children parents command]
    (case (nth command 0)
      :set-value (let [[_ val] command]
                   (-> node
                       (assoc :state val)
                       (update :reset-vars assoc (v-sel ns) val))))))

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

