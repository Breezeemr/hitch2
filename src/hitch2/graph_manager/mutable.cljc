(ns hitch2.graph-manager.mutable
  (:require [hitch2.protocols.graph-manager :as g]))

(defn apply-command [graph-manager-value machine command])

(deftype gm [state]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerSync
  (-transact! [graph-manager machine command]
    (:graph-value (swap! state apply-command machine command)))
  (-transact-commands! [graph-manager cmds])
  g/GraphManagerAsync
  (-transact-async! [graph-manager v command])
  (-transact-commands-async! [graph-manager cmds])
  )

(defn make-gm []
  (->gm (atom {:graph-value {}
               :node-state  {}
               :parents     {}
               :children    {}})))
