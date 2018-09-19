(ns hitch2.protocols.graph-manager)

(defprotocol GraphManagerSync
  (-transact!
    [graph-manager machine command]

    "Apply a transaction to a graph manager and mutate the graph.

     This operation should mutate the graph manager so it contains the result
     of its internal graph-node after applying the commands. The graph manager
     must also ensure that any effects from the operation are executed and that
     any external-children are recalculated.

     It must return a promise:
         derefing the promise returns a graph value or throws an exception")
  (-transact-commands!
    [graph-manager cmds]
    "Apply a transaction to a graph manager and mutate the graph.

     This operation should mutate the graph manager so it contains the result
     of its internal graph-node after applying the commands. The graph manager
     must also ensure that any effects from the operation are executed and that
     any external-children are recalculated.
    It must return a promise:
             derefing the promise returns a graph value or throws an exception"))

(defprotocol GraphManagerAsync
  (-transact-async!
    [graph-manager v command]

    "Apply a transaction to a graph manager and mutate the graph.

     This operation should mutate the graph manager so it contains the result
     of its internal graph-node after applying the commands. The graph manager
     must also ensure that any effects from the operation are executed and that
     any external-children are recalculated.

     It must return a promise:
         derefing the promise returns a graph value or throws an exception")
  (-transact-commands-async!
    [graph-manager cmds]
    "Apply a transaction to a graph manager and mutate the graph.

     This operation should mutate the graph manager so it contains the result
     of its internal graph-node after applying the commands. The graph manager
     must also ensure that any effects from the operation are executed and that
     any external-children are recalculated.
    It must return a promise:
             derefing the promise returns a graph value or throws an exception"))

(defprotocol Snapshot
  (-get-graph [x] "gets a snapshot from a graph-manger"))



(defmulti run-effect (fn [graph-manager effect] (:type effect)))

