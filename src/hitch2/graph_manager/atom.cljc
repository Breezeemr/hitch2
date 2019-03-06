(ns hitch2.graph-manager.atom
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.scheduler.normal :refer [default-scheduler]]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.graph-manager.core :refer [apply-command apply-commands ->GraphManagerValue]]))

(defn schedule-effects [scheduler graph-manager effect-atom]
  (when-some [{:keys [sync-effects
                      async-effects]} @effect-atom]
    (g/-run-sync scheduler graph-manager sync-effects)
    (g/-run-async scheduler graph-manager async-effects)))

(defn queue-effect-fn [effect-atom]
  (fn [sync-effects async-effects]
    (reset! effect-atom
      {:sync-effects sync-effects
       :async-effects async-effects})))

(deftype gm [state scheduler resolver]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerSync
  (-transact! [graph-manager curator command]
    (let [effects (atom nil)]
      (swap! state apply-command resolver curator command
        (queue-effect-fn effects))
      (schedule-effects scheduler graph-manager effects))
    (:graph-value @state))
  (-transact-commands! [graph-manager cmds]
    (let [effects (atom nil)]
      (swap! state apply-commands resolver cmds
        (queue-effect-fn effects))
      (schedule-effects scheduler graph-manager effects))
    (:graph-value @state))
  g/GraphManagerAsync
  (-transact-async! [graph-manager v command])
  (-transact-commands-async! [graph-manager cmds])
  g/Inspect
  (-observed-by [gm descriptor]
    (get-in @state [:observed-by descriptor] NOT-IN-GRAPH-SENTINEL))
  (-observes [gm descriptor]
    (let [node-state (get-in @state [:node-state descriptor] NOT-IN-GRAPH-SENTINEL)]
      (if (identical? node-state NOT-IN-GRAPH-SENTINEL)
        NOT-IN-GRAPH-SENTINEL
        (:observes node-state))))
  g/Resolver
  (-get-resolver [gm] resolver))

(defn make-gm
  ([resolver] (make-gm resolver default-scheduler))
  ([resolver scheduler]
   (->gm (atom (->GraphManagerValue
                {}
                {}
                (transient (hash-map))))
         scheduler
     resolver)))


