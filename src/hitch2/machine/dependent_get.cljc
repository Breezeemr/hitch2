(ns hitch2.machine.dependent-get
  (:require [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]))

(defrecord node-state [state change-parent reset-vars
                       async-effects sync-effects])
(def dget-machine
  (reify
    machine-proto/Init
    (-initialize [machine-instance]
      (machine-proto/->node-state {} nil nil [] []))
    machine-proto/ParentChanges
    (-parent-value-changes [_ graph-value node children parents parent-selectors]
      (let [selector->targets (:state node)]
        (update node :async-effects
          into
          (mapcat (fn [selector]

                    (for [target (selector->targets selector)]
                      {:type   :notify
                       :target target
                       :value  selector})))
          parent-selectors)))
    machine-proto/Commandable
    (-apply-command [_ graph-value node children parents command]
      (case (nth command 0)
        :dget-subscribe
        (let [[_ selector target] command]
          (-> node
              (update-in [:state selector] (fnil conj #{}) target)
              (update :change-parent assoc selector true)))
        :dget-unsubscribe
        (let [[_ selector target] command]
          (let [new-node (update-in node [:state selector] (fnil disj #{}) target)]
            (if (not-empty (get-in new-node [:state selector]))
              new-node
              (update :change-parent assoc selector false))))))))


(defmethod graph-proto/run-effect :notify [effect]
  (prn effect))

