(ns hitch2.curator.hook
  (:require [hitch2.def.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor :as descriptor]
            [hitch2.selector-impl-registry :as reg]))

(def hook-spec
  {:hitch2.descriptor/name ::hook
   :hitch2.descriptor.spec/kind :machine})

(defrecord node-state [state change-focus set-projections
                       async-effects sync-effects])
(def initial-node (assoc machine-proto/initial-curator-state :state {}))

(defn remove-called-hooks [state selectors]
  (reduce dissoc state selectors))

(def-descriptor-spec hook-machine-spec
  :machine
  :hitch2.descriptor.spec/canonical-form
  :hitch2.descriptor.spec.canonical-form/positional)

(def hook-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/machine
   ::machine-proto/init (fn [machine-selector] initial-node)
   ::machine-proto/observed-value-changes
                             (fn [machine-selector graph-value node parent-selectors]
                               (let [selector->targets (:state node)]
                                 (-> node
                                     (update :async-effects
                                       into
                                       (mapcat (fn [selector]
                                                 (for [target (seq (selector->targets selector))]
                                                   {:type     :hook-call
                                                    :target   target
                                                    :selector selector})))
                                       parent-selectors)
                                     (update :state remove-called-hooks parent-selectors))))
   ::machine-proto/apply-command
                             (fn [machine-selector graph-value node command]
                               (case (nth command 0)
                                 :hook-subscribe
                                 (let [[_ selector target] command]
                                   (-> node
                                       (update-in [:state selector] (fnil conj #{}) target)
                                       (update :change-focus assoc selector true)))
                                 :hook-unsubscribe
                                 (let [[_ selector target] command]
                                   (let [new-node (update-in node [:state selector] (fnil disj #{}) target)]
                                     (if (not-empty (get-in new-node [:state selector]))
                                       new-node
                                       (update new-node :change-focus assoc selector false))))))})

(reg/def-registered-selector hook-machine-spec' hook-machine-spec hook-impl)

(def hook-machine
  (descriptor/dtor  hook-machine-spec'))


(def-descriptor-spec hook-change-machine-spec
  :machine
  :hitch2.descriptor.spec/canonical-form
  :hitch2.descriptor.spec.canonical-form/positional)

(def hook-change-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/machine
   ::machine-proto/init (fn [machine-selector] initial-node)
   ::machine-proto/observed-value-changes
   (fn [machine-selector graph-value node parent-selectors]
     (let [selector->targets (:state node)]
       (-> node
           (update :async-effects
             into
             (mapcat (fn [selector]
                       (for [target (seq (selector->targets selector))]
                         {:type     :hook-changes-call
                          :target   target
                          :selector selector})))
             parent-selectors))))
   ::machine-proto/apply-command
   (fn [machine-selector graph-value node command]
     (case (nth command 0)
       :hook-change-subscribe
       (let [[_ selector target] command
             current-selector-value (get graph-value selector NOT-FOUND-SENTINEL)]
         (cond->
           (-> node
               (update-in [:state selector] (fnil conj #{}) target)
               (update :change-focus assoc selector true))
           (not (identical? current-selector-value NOT-FOUND-SENTINEL))
           (update :sync-effects conj {:type     :hook-changes-call
                                       :target   target
                                       :selector selector})))
       :hook-change-unsubscribe
       (let [[_ selector target] command]
         (let [new-node (update-in node [:state selector] disj target)]
           (if (not-empty (get-in new-node [:state selector]))
             new-node
             (update new-node :change-focus assoc selector false))))))})

(reg/def-registered-selector hook-change-machine-spec' hook-change-machine-spec hook-change-impl)


(def hook-change-machine
  (descriptor/dtor  hook-change-machine-spec'))

(defmethod graph-proto/run-effect :hook-call [graph-manager
                                              {:as effect
                                               f :target
                                               sel :selector}]
  (let [graph-value (graph-proto/-get-graph graph-manager)
        v (get graph-value sel)]
    (f v)))

(defmethod graph-proto/run-effect :hook-changes-call [graph-manager
                                              {:as effect
                                               f :target
                                               sel :selector}]
  (let [graph-value (graph-proto/-get-graph graph-manager)
        v (get graph-value sel)]
    (f v)))

