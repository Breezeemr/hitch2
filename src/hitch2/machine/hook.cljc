(ns hitch2.machine.hook
  (:require [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.selector :as sel-proto]))

(defrecord node-state [state change-parent reset-vars
                       async-effects sync-effects])
(def initial-node (assoc machine-proto/initial-node :state {}))

(def impl (reify
            sel-proto/ImplementationKind
            (-imp-kind [machine] :hitch.selector.kind/machine)))

(defn remove-called-hooks [state selectors]
  (reduce dissoc state selectors))

(def hook-machine
  (reify
    sel-proto/SelectorImplementation
    (-imp [machine-instance] impl)
    machine-proto/Init
    (-initialize [machine-instance] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ graph-value node children parents parent-selectors]
      (let [selector->targets (:state node)]
        (-> node
            (update :async-effects
              into
              (mapcat (fn [selector]
                        (for [target (seq (selector->targets selector))]
                          {:type   :hook-call
                           :target target
                           :selector selector})))
              parent-selectors)
            (update :state remove-called-hooks parent-selectors))))
    machine-proto/Commandable
    (-apply-command [_ graph-value node children parents command]
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
              (update  new-node :change-parent assoc selector false))))))))

(def hook-change-machine
  (reify
    sel-proto/SelectorImplementation
    (-imp [machine-instance] impl)
    machine-proto/Init
    (-initialize [machine-instance] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ graph-value node children parents parent-selectors]
      (let [selector->targets (:state node)]
        (-> node
            (update :async-effects
              into
              (mapcat (fn [selector]
                        (for [target (seq (selector->targets selector))]
                          {:type   :hook-changes-call
                           :target target
                           :selector selector})))
              parent-selectors))))
    machine-proto/Commandable
    (-apply-command [_ graph-value node children parents command]
      (case (nth command 0)
        :hook-change-subscribe
        (let [[_ selector target] command
              current-selector-value (get graph-value selector NOT-FOUND-SENTINEL)]
          (cond->
            (-> node
                (update-in [:state selector] (fnil conj #{}) target)
                (update :change-parent assoc selector true))
            (not (identical? current-selector-value NOT-FOUND-SENTINEL))
            (update :sync-effects conj {:type   :hook-changes-call
                                 :target target
                                 :selector selector})))
        :hook-change-unsubscribe
        (let [[_ selector target] command]
          (let [new-node (update-in node [:state selector] disj target)]
            (if (not-empty (get-in new-node [:state selector]))
              new-node
              (update new-node :change-parent assoc selector false))))))))

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

