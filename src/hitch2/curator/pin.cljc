(ns hitch2.curator.pin
  (:require [hitch2.def.curator :as curator-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor :as descriptor]
            [hitch2.descriptor-impl-registry :as reg]))

(def initial-node (assoc curator-proto/initial-curator-state :state #{}))

(def-descriptor-spec pin-curator-spec
  :curator)

(def pin-curator-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/curator
   ::curator-proto/init (fn [curator-descriptor] initial-node)
   ::curator-proto/observed-value-changes
                             (fn [curator-descriptor graph-value node parent-descriptors]
                               node)
   ::curator-proto/apply-command
                             (fn [curator-descriptor graph-value node command]
                               (case (nth command 0)
                                 :pin
                                 (let [[_ descriptor] command
                                       there? (get-in node [:state descriptor])]
                                   (if there?
                                     node
                                     (-> node
                                         (update :state conj descriptor)
                                         (update :change-focus assoc descriptor true))))
                                 :unpin
                                 (let [[_ descriptor] command
                                       there? (get-in node [:state descriptor])]
                                   (if there?
                                     (-> node
                                         (update :state disj descriptor)
                                         (update :change-focus assoc descriptor false))
                                     node))))})

(reg/def-registered-descriptor pin-curator-spec' pin-curator-spec pin-curator-impl)

(def pin-curator
  (descriptor/->dtor  pin-curator-spec' nil))

