(ns hitch2.curator.hook
  (:require [hitch2.protocols.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.selector :as sel-proto
             :refer [def-selector-spec]]
            [hitch2.selector-impl-registry :as reg]))

(def hook-spec
  {:hitch.selector/name ::hook
   :hitch.selector.spec/kind :machine})

(defrecord node-state [state change-parent reset-vars
                       async-effects sync-effects])
(def initial-node (assoc machine-proto/initial-machine-state :state {}))

(defn remove-called-hooks [state selectors]
  (reduce dissoc state selectors))

(def-selector-spec hook-machine-spec
  :machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/positional)

(def hook-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [machine] :hitch.selector.kind/machine)
    machine-proto/Init
    (-initialize [machine-instance machine-selector] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ machine-selector graph-value node children parents parent-selectors]
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
    machine-proto/Commandable
    (-apply-command [_ machine-selector graph-value node children parents command]
      (case (nth command 0)
        :hook-subscribe
        (let [[_ selector target] command]
          (-> node
              (update-in [:state selector] (fnil conj #{}) target)
              (update :change-parent assoc selector true)))
        :hook-unsubscribe
        (let [[_ selector target] command]
          (let [new-node (update-in node [:state selector] (fnil disj #{}) target)]
            (if (not-empty (get-in new-node [:state selector]))
              new-node
              (update new-node :change-parent assoc selector false))))))))

(reg/def-registered-selector hook-machine-spec' hook-machine-spec hook-impl)

(def hook-machine
  (sel-proto/sel hook-machine-spec'))


(def-selector-spec hook-change-machine-spec
  :machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/positional)

(def hook-change-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [machine] :hitch.selector.kind/machine)
    machine-proto/Init
    (-initialize [machine-instance machine-selector] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ machine-selector graph-value node children parents parent-selectors]
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
    machine-proto/Commandable
    (-apply-command [_ machine-selector graph-value node children parents command]
      (case (nth command 0)
        :hook-change-subscribe
        (let [[_ selector target] command
              current-selector-value (get graph-value selector NOT-FOUND-SENTINEL)]
          (cond->
            (-> node
                (update-in [:state selector] (fnil conj #{}) target)
                (update :change-parent assoc selector true))
            (not (identical? current-selector-value NOT-FOUND-SENTINEL))
            (update :sync-effects conj {:type     :hook-changes-call
                                        :target   target
                                        :selector selector})))
        :hook-change-unsubscribe
        (let [[_ selector target] command]
          (let [new-node (update-in node [:state selector] disj target)]
            (if (not-empty (get-in new-node [:state selector]))
              new-node
              (update new-node :change-parent assoc selector false))))))))

(reg/def-registered-selector hook-change-machine-spec' hook-change-machine-spec hook-change-impl)


(def hook-change-machine
  (sel-proto/sel hook-change-machine-spec'))

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

