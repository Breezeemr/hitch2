(ns hitch2.machine.mutable-var
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.selector-impl-registry :as reg]
            [hitch2.protocols.selector :as sel-proto]))

(declare mutable-var)
(def initial-node (assoc machine-proto/initial-machine-state :state NOT-FOUND-SENTINEL))

(def machine-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [machine] :hitch.selector.kind/machine)
    machine-proto/Init
    (-initialize [machine-instance machine-selector] initial-node)
    machine-proto/ChildChanges
    (-child-changes [machine-instance machine-selector graph-value node children parents children-added children-removed]
      node)
    machine-proto/Commandable
    (-apply-command [_ machine-selector graph-value node children parents command]
      (case (nth command 0)
        :set-value (let [[_ val] command]
                     (-> node
                         (assoc :state val)
                         (update :reset-vars assoc (mutable-var (:name machine-selector)) val)))
        :clear (-> node
                   (assoc :state NOT-FOUND-SENTINEL)
                   (update :reset-vars assoc (mutable-var (:name machine-selector)) NOT-FOUND-SENTINEL))))))

(reg/def-registered-selector mutable-machine-name "mutable var machine" machine-impl)

(defrecord mutable-machine [name]
  sel-proto/SelectorName
  (-sname [selector] mutable-machine-name))

(def var-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [var]
      :hitch.selector.kind/var)
    sel-proto/GetMachine
    (-get-machine [var sel]
      (->mutable-machine (:a sel)))))
(reg/def-registered-selector mutable-machine-var "mutable var" var-impl)

(defn mutable-var [var-name]
  (sel-proto/->Selector1 mutable-machine-var var-name))

