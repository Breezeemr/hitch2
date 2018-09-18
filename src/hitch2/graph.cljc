(ns hitch2.graph
  (:require [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.machine.dependent-get :refer [dget-machine]]))


(defn hook-sel
  "Call fn `cb` once with the value of `selector` in `graph` as soon as it is
  available. `cb` may be called synchronously if the selector's value is already
  known."
  [graph cb selector]
  
  nil)

(defn hook-change-sel
  "Call fn `cb` with the value of `selector` in `graph` as soon as it is
  available, and every time the value changes. `cb` may be called synchronously
  if the selector's value is already known and the graph supports
  eager selector resolution.

  Returns a zero-arg unsubscribe function. After it is called, cb will not
  be called again.

  There is no guarantee that each `cb` call will receive a value not= to the
  previous call's value."
  [graph cb selector]
  )
(defn get-target-for-tx-context [tx]
  :target)

(defn dget-sel!
  "Return the value (or `nf` if not yet known) for a selector from graph
  transaction context `tx`."
  [tx selector nf]
  (let [graph (graph-proto/-transact! tx dget-machine [:dget-subscribe selector (get-target-for-tx-context tx)])]
    (get graph selector nf)))

(defn hook
  "Call fn `cb` once with the value of selector returned from
  selector-constructor and remaining arguments in `graph` as soon as it is
  available. `cb` may be called synchronously if the selector's value is already
  known."
  ([graph cb selector-constructor] (hook-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-sel graph cb (selector-constructor a b c d f g h))))

(defn hook-change
  "Like hook-change-sel, but receives a selector-constructor plus arguments
  instead of a selector."
  ([graph cb selector-constructor] (hook-change-sel graph cb (selector-constructor)))
  ([graph cb selector-constructor a] (hook-change-sel graph cb (selector-constructor a)))
  ([graph cb selector-constructor a b] (hook-change-sel graph cb (selector-constructor a b)))
  ([graph cb selector-constructor a b c] (hook-change-sel graph cb (selector-constructor a b c)))
  ([graph cb selector-constructor a b c d] (hook-change-sel graph cb (selector-constructor a b c d)))
  ([graph cb selector-constructor a b c d f] (hook-change-sel graph cb (selector-constructor a b c d f)))
  ([graph cb selector-constructor a b c d f g] (hook-change-sel graph cb (selector-constructor a b c d f g)))
  ([graph cb selector-constructor a b c d f g h] (hook-change-sel graph cb (selector-constructor a b c d f g h))))

(defn dget!
  "Return the value (or `nf` if not yet known) for a selector-constructor and
  its arguments from graph transaction context `tx`."
  ([tx nf selector-constructor]
   )
  ([tx nf selector-constructor a]
   )
  ([tx nf selector-constructor a b]
   )
  ([tx nf selector-constructor a b c]
   )
  ([tx nf selector-constructor a b c d]
   )
  ([tx nf selector-constructor a b c d e]
   )
  ([tx nf selector-constructor a b c d e f]
   )
  ([tx nf selector-constructor a b c d e f g]
   ))

(defn select-sel!
  "Return a box containing the value for a selector from graph transaction
  context `tx`. The returned box is the same as that returned by `select!`."
  ([tx selector]
   ;; todo 
   ))

(defn select!
  "Return a box containing the value for a selector-constructor and its
  arguments from graph transaction context `tx`. Retrieve the value with deref
  (@). If the value is not yet known, deref will throw an exception which the
  transaction context will catch. You can test if a value is available
  using `(realized? box)`."
  ([tx selector-constructor]
   ;; todo
   )
  ([tx selector-constructor a]
   ;;todo
   )
  ([tx selector-constructor a b]
   )
  ([tx selector-constructor a b c]
   )
  ([tx selector-constructor a b c d]
   )
  ([tx selector-constructor a b c d e]
   )
  ([tx selector-constructor a b c d e f]
   )
  ([tx selector-constructor a b c d e f g]
   ))


(defn hitch-callback
  "Given a graph, execute fn `body` in a graph transaction context, calling
  fn `cb` with the result of body when available. `body` will be called like
  `(apply body tx extra-args-to-hitch-callback)`. Body may use select! and
  may be called multiple times.

  If body uses `select!` and derefs unavailable values, the exception will
  be caught and body will be called again when the value is available.
  This continues until body returns without throwing an exception."
  ([graph cb body]
   )
  ([graph cb body a]
   )
  ([graph cb body a b]
   )
  ([graph cb body a b c]
   )
  ([graph cb body a b c d]
   )
  ([graph cb body a b c d e]
   )
  ([graph cb body a b c d e f]
   )
  ([graph cb body a b c d e f g]
   ))

(defn apply-commands
  "Issue a list of selector-command pairs to a graph. Selector-command-pairs
   is like `[[Selector [command & command-args]] ,,,]`. Do not rely on returned
   value."
  [graph selector-command-pairs]
  )

