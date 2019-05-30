(ns hitch2.graph-manager.atom
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.scheduler.normal :refer [default-process-manager]]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.graph-manager.core :refer [apply-command apply-commands
                                                ->GraphManagerValue send-messages!]]))

(deftype gm [state scheduler resolver]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerSync
  (-transact! [graph-manager curator command]
    (swap! state apply-command resolver curator command)
    (:graph-value @state))
  (-transact-commands! [graph-manager cmds]
    (swap! state apply-commands resolver cmds)
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
  ([resolver] (make-gm resolver default-process-manager))
  ([resolver pm]
   (let [gmv (atom (->GraphManagerValue
                     {}
                     {}
                     (transient (hash-map))
                     []))
         gm (->gm gmv
              pm
              resolver)]
     (add-watch gmv ::watch
       (fn [_key atom-ref old-value new-value]
         (when-some [messages (not-empty (:outbox new-value))]
           (send-messages! new-value pm gm messages))))
     gm)))


