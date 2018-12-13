(ns hitch2.curator.mutable-var
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.selector-impl-registry :as reg]
            [hitch2.sel :as sel]
            [hitch2.protocols.selector :as sel-proto
             :refer [def-selector-spec]]))

(declare mutable-var)
(def initial-node (assoc machine-proto/initial-curator-state :state NOT-FOUND-SENTINEL))

(def-selector-spec mutable-var-machine-spec
  :machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/map
  :hitch.selector.spec/positional-params
  [:var-name])

(def machine-impl
  {:hitch.selector.impl/kind :hitch.selector.kind/machine
   ::machine-proto/init      (fn [machine-selector] initial-node)
   ::machine-proto/curation-changes
                             (fn [machine-selector graph-value node children-added children-removed]
                               (assoc node
                                 :set-projections
                                 (-> (:set-projections node)
                                     (into
                                       (keep
                                         (fn [x]
                                           (when-not (identical? (:state node) NOT-FOUND-SENTINEL)
                                             [(mutable-var (:var-name (:value machine-selector))) (:state node)])))
                                       children-added))))
   ::machine-proto/apply-command
                             (fn [machine-selector graph-value node command]
                               (case (nth command 0)
                                 :set-value (let [[_ val] command]
                                              (-> node
                                                  (assoc :state val)
                                                  (update :set-projections assoc (mutable-var (:var-name (:value machine-selector))) val)))
                                 :clear (-> node
                                            (assoc :state NOT-FOUND-SENTINEL)
                                            (update :set-projections assoc (mutable-var (:var-name (:value machine-selector))) NOT-FOUND-SENTINEL))))})

(reg/def-registered-selector mutable-var-machine-spec' mutable-var-machine-spec machine-impl)

(defn mutable-machine [var-name]
  (sel/map->sel mutable-var-machine-spec' {:var-name var-name}))

(def-selector-spec mutable-var-spec
  :not-machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/map
  :hitch.selector.spec/positional-params
  [:var-name])

(def mutable-var-impl
  {:hitch.selector.impl/kind :hitch.selector.kind/var
   :hitch.selector.impl/get-machine
                             (fn [sel]
                               (mutable-machine (:var-name (:value sel))))})

(reg/def-registered-selector mutable-var-spec' mutable-var-spec mutable-var-impl)

(defn mutable-var [var-name]
  (sel/map->sel
    mutable-var-spec'
    {:var-name var-name}))

