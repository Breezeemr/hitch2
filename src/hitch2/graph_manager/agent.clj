(ns hitch2.graph-manager.agent
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.scheduler.normal :refer [default-process-manager]]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.graph-manager.core :refer [apply-command apply-commands ->GraphManagerValue
                                                send-messages!]]))

(defn queue-effect-fn [graph-manager]
  (fn [sync-effects async-effects]
    (reset! (.-queued-effects graph-manager)
      {:sync-effects sync-effects
       :async-effects async-effects})))

(deftype gm [state pm resolver]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerAsync
  (-transact-async! [graph-manager curator command]
    (send state apply-command resolver curator command))
  (-transact-commands-async! [graph-manager cmds]
    (send state apply-commands resolver cmds))
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
  ([resolver] (make-gm resolver (default-process-manager resolver)))
  ([resolver pm]
   (let [gmv (agent (->GraphManagerValue
                     {}
                     {}
                     (transient (hash-map))
                      []))
         graph-manager (->gm gmv
                         pm
                         resolver)]
     (add-watch gmv ::watch
       (fn [_key agent-ref old-value new-value]
         (when-some [messages (not-empty (:outbox new-value))]
           (send-messages! new-value pm gm messages))))
     graph-manager)))


