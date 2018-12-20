(ns hitch2.curator.pin
  (:require [hitch2.def.curator :as curator-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor :as descriptor]
            [hitch2.selector-impl-registry :as reg]))

(def initial-node (assoc curator-proto/initial-curator-state :state #{}))

(def-descriptor-spec pin-curator-spec
  :curator)

(def pin-curator-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/curator
   ::curator-proto/init (fn [curator-selector] initial-node)
   ::curator-proto/observed-value-changes
                             (fn [curator-selector graph-value node parent-selectors]
                               node)
   ::curator-proto/apply-command
                             (fn [curator-selector graph-value node command]
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

(reg/def-registered-selector pin-curator-spec' pin-curator-spec pin-curator-impl)

(def pin-curator
  (descriptor/->dtor  pin-curator-spec' nil))

