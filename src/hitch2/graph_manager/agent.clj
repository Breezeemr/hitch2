(ns hitch2.graph-manager.agent
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.scheduler.normal :refer [default-scheduler]]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.graph-manager.core :refer [apply-command apply-commands ->GraphManagerValue]]))


(defn transact [state graph-manager  resolver scheduler curator command]
  (let [sync-effects-atom (volatile! (transient []))
        async-effects-atom (volatile! (transient []))]
    (send state apply-command resolver curator command sync-effects-atom async-effects-atom)
    (g/-run-sync scheduler graph-manager (persistent! @sync-effects-atom))
    (g/-run-async scheduler graph-manager (persistent! @async-effects-atom))))

(defn transact-cmds [state graph-manager resolver scheduler cmds]
  (let [sync-effects-atom (volatile! (transient []))
        async-effects-atom (volatile! (transient []))]
    (send state apply-commands resolver cmds sync-effects-atom async-effects-atom)
    (g/-run-sync scheduler graph-manager (persistent! @sync-effects-atom))
    (g/-run-async scheduler graph-manager (persistent! @async-effects-atom))))

(deftype gm [state scheduler resolver]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerAsync
  (-transact-async! [graph-manager curator command]
    (transact state  graph-manager resolver scheduler curator command))
  (-transact-commands-async! [graph-manager cmds]
    (transact-cmds state graph-manager resolver scheduler cmds))
  g/Inspect
  (-observed-by [gm descriptor]
    (get-in @state [:observed-by descriptor] NOT-IN-GRAPH-SENTINEL))
  (-observes [gm descriptor]
    (let [node-state (get-in @state [:node-state descriptor] NOT-IN-GRAPH-SENTINEL)]
      (if (identical? node-state NOT-IN-GRAPH-SENTINEL)
        NOT-IN-GRAPH-SENTINEL
        (:observes node-state))))
  g/Resolver
  (-get-resolver [gm] resolver)
  )

(defn make-gm
  ([resolver] (make-gm resolver default-scheduler))
  ([resolver scheduler]
   (->gm (atom (->GraphManagerValue
                {}
                {}
                (transient (hash-map))))
         scheduler
     resolver)))


