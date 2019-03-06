(ns hitch2.graph-manager.agent
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.scheduler.normal :refer [default-scheduler]]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.graph-manager.core :refer [transact transact-cmds ->GraphManagerValue]]))

(deftype gm [state scheduler resolver]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerAsync
  (-transact-async! [graph-manager curator command]
    (send state transact graph-manager resolver scheduler curator command))
  (-transact-commands-async! [graph-manager cmds]
    (send state transact-cmds  graph-manager resolver scheduler cmds))
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


