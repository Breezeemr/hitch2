(ns hitch2.curator.mutable-var
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.def.curator :as curator-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.descriptor-impl-registry :as reg]
            [hitch2.descriptor :as descriptor]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]))

(declare mutable-var)
(def initial-node (assoc curator-proto/initial-curator-state :state NOT-FOUND-SENTINEL))

(def-descriptor-spec mutable-var-curator-spec
  :curator
  :canonical-form :map
  :positional-params [:var-name])

(def curator-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/curator
   ::curator-proto/init      (fn [curator-descriptor] initial-node)
   ::curator-proto/curation-changes
                             (fn [curator-descriptor graph-value node children-added children-removed]
                               (assoc node
                                 :set-projections
                                 (-> (:set-projections node)
                                     (into
                                       (keep
                                         (fn [x]
                                           (when-not (identical? (:state node) NOT-FOUND-SENTINEL)
                                             [(mutable-var (:var-name (:term curator-descriptor))) (:state node)])))
                                       children-added))))
   ::curator-proto/apply-command
                             (fn [curator-descriptor graph-value node command]
                               (case (nth command 0)
                                 :set-value (let [[_ val] command]
                                              (-> node
                                                  (assoc :state val)
                                                  (update :set-projections assoc (mutable-var (:var-name (:term curator-descriptor))) val)))
                                 :clear (-> node
                                            (assoc :state NOT-FOUND-SENTINEL)
                                            (update :set-projections assoc (mutable-var (:var-name (:term curator-descriptor))) NOT-FOUND-SENTINEL))))})

(reg/def-registered-descriptor mutable-var-curator-spec' mutable-var-curator-spec curator-impl)

(defn mutable-curator [var-name]
  (descriptor/->dtor  mutable-var-curator-spec' {:var-name var-name}))

(def-descriptor-spec mutable-var-spec
  :not-curator
  :canonical-form :map
  :positional-params [:var-name])

(def mutable-var-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator
                             (fn [sel]
                               (mutable-curator (:var-name (:term sel))))})

(reg/def-registered-descriptor mutable-var-spec' mutable-var-spec mutable-var-impl)

(defn mutable-var [var-name]
  (descriptor/->dtor
    mutable-var-spec'
    {:var-name var-name}))

