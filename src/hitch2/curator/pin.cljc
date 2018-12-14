(ns hitch2.curator.pin
  (:require [hitch2.def.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor :as descriptor]
            [hitch2.selector-impl-registry :as reg]))

(def initial-node (assoc machine-proto/initial-curator-state :state #{}))

(def-descriptor-spec pin-machine-spec
  :machine
  :hitch2.descriptor.spec/canonical-form
  :hitch2.descriptor.spec.canonical-form/positional)

(def pin-machine-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/machine
   ::machine-proto/init (fn [machine-selector] initial-node)
   ::machine-proto/observed-value-changes
                             (fn [machine-selector graph-value node parent-selectors]
                               node)
   ::machine-proto/apply-command
                             (fn [machine-selector graph-value node command]
                               (case (nth command 0)
                                 :pin
                                 (let [[_ selector] command
                                       there? (get-in node [:state selector])]
                                   (if there?
                                     node
                                     (-> node
                                         (update :state conj selector)
                                         (update :change-focus assoc selector true))))
                                 :unpin
                                 (let [[_ selector] command
                                       there? (get-in node [:state selector])]
                                   (if there?
                                     (-> node
                                         (update :state disj selector)
                                         (update :change-focus assoc selector false))
                                     node))))})

(reg/def-registered-selector pin-machine-spec' pin-machine-spec pin-machine-impl)

(def pin-machine
  (descriptor/dtor  pin-machine-spec'))

