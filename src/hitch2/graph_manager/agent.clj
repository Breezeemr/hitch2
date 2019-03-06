(ns hitch2.graph-manager.agent
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.scheduler.normal :refer [default-scheduler]]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.graph-manager.core :refer [apply-command apply-commands ->GraphManagerValue]]))

(defn queue-effect-fn [graph-manager]
  (fn [sync-effects async-effects]
    (reset! (.-queued-effects graph-manager)
      {:sync-effects sync-effects
       :async-effects async-effects})))

(deftype gm [state scheduler resolver ^:mutable queued-effects]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerAsync
  (-transact-async! [graph-manager curator command]
    (send state apply-command resolver curator command (queue-effect-fn graph-manager)))
  (-transact-commands-async! [graph-manager cmds]
    (send state apply-commands resolver cmds (queue-effect-fn graph-manager)))
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
   (let [gmv (agent (->GraphManagerValue
                     {}
                     {}
                     (transient (hash-map))))
         graph-manager (->gm gmv
                         scheduler
                         resolver
                         (atom nil))]
     (add-watch gmv ::watch
       (fn [_key agent-ref old-value new-value]
         (when-some [{:keys [sync-effects
                           async-effects]} @(.-queued-effects graph-manager)]
           (reset! (.-queued-effects graph-manager) nil)
           (g/-run-sync scheduler graph-manager sync-effects)
           (g/-run-async scheduler graph-manager async-effects)
           )))
     graph-manager)))


