(ns hitch2.machine.hook
  (:require [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto]))

(defrecord node-state [state change-parent reset-vars
                       async-effects sync-effects])
(def initial-node (assoc machine-proto/initial-node :state {}))

(def impl (reify
            sel-proto/ImplementationKind
            (-imp-kind [machine] :hitch.selector.kind/machine)))

(def hook-machine
  (reify
    sel-proto/SelectorImplementation
    (-imp [machine-instance] impl)
    machine-proto/Init
    (-initialize [machine-instance] initial-node)
    machine-proto/ParentChanges
    (-parent-value-changes [_ graph-value node children parents parent-selectors]
      (let [selector->targets (:state node)]
        (update node :async-effects
          into
          (mapcat (fn [selector]

                    (for [target (selector->targets selector)]
                      {:type   :hook-call
                       :target target
                       :value  selector})))
          parent-selectors)))
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
              (update :change-parent assoc selector false))))))))


(defmethod graph-proto/run-effect :hook-call [graph-manager
                                              {:as effect
                                               f :target
                                               sel :selector}]
  (let [graph-value (graph-proto/-get-graph graph-manager)
        v (get graph-value sel)]
    (prn effect)
    (f graph-manager v)))

