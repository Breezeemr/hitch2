(ns hitch2.graph-manager.mock
  (:require  [hitch2.protocols.graph-manager :as g]))

(defrecord mock-gm [state commands]
  g/Snapshot
  (-get-graph [graph-manager]
    @state)
  g/GraphManagerSync
  (-transact! [graph-manager curator command]
    (swap! commands conj [curator command])
    @state)
  (-transact-commands! [graph-manager cmds]
    (swap! commands into cmds)

    @state)
  g/GraphManagerAsync
  (-transact-async! [graph-manager curator command]
    (swap! commands conj [curator command])
    nil)
  (-transact-commands-async! [graph-manager cmds]
    (swap! commands into cmds)
    nil))

(defn make-gm []
  (->mock-gm (atom {})
    (atom [])))


