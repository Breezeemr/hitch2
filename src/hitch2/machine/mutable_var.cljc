(ns hitch2.machine.mutable-var
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.selector-impl-registry :as reg]
            [hitch2.protocols.selector :as sel-proto
             :refer [def-selector-spec]]))

(declare mutable-var)
(def initial-node (assoc machine-proto/initial-machine-state :state NOT-FOUND-SENTINEL))

(def-selector-spec mutable-var-machine-spec
  :hitch.selector.spec.kind/map-param
  :hitch.selector.spec/positional-params
  [:var-name])

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
                         (update :reset-vars assoc (mutable-var (:var-name machine-selector)) val)))
        :clear (-> node
                   (assoc :state NOT-FOUND-SENTINEL)
                   (update :reset-vars assoc (mutable-var (:var-name machine-selector)) NOT-FOUND-SENTINEL))))))

(reg/def-registered-selector mutable-var-machine-spec' mutable-var-machine-spec machine-impl)

(defn mutable-machine [var-name]
  (sel-proto/map->sel mutable-var-machine-spec' {:var-name var-name}))

(def-selector-spec mutable-var-spec
  :hitch.selector.spec.kind/map-param
  :hitch.selector.spec/positional-params
  [:var-name])

(def mutable-var-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [var]
      :hitch.selector.kind/var)
    sel-proto/GetMachine
    (-get-machine [var sel]
      (mutable-machine (:var-name sel)))))

(reg/def-registered-selector mutable-var-spec' mutable-var-spec mutable-var-impl)

(defn mutable-var [var-name]
  (sel-proto/map->sel
    mutable-var-spec'
    {:var-name var-name}))

