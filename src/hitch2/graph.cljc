(ns hitch2.graph
  (:require [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.machine.dependent-get :refer [dget-machine]]
            [hitch2.machine.hook :refer [hook-machine]]
            [hitch2.protocols.tx-manager :as tx-proto]
            [hitch2.protocols :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
            [hitch2.halt :as halt]))

(defn get-target-for-tx-context [tx]
  :target)

(defn hook-sel
  "Call fn `cb` once with the value of `selector` in `graph` as soon as it is
  available. `cb` may be called synchronously if the selector's value is already
  known."
  [graph-manager cb selector]
  (let [graph-value (graph-proto/-get-graph graph-manager)
        val  (get graph-value selector NOT-IN-GRAPH-SENTINEL)]
    (if (identical? val NOT-IN-GRAPH-SENTINEL)
      (graph-proto/-transact! graph-manager hook-machine [:hook-subscribe selector cb])
      (cb graph-manager val)))
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
  [graph-manager cb selector]
  )

(defn hook
  "Call fn `cb` once with the value of selector returned from
  selector-constructor and remaining arguments in `graph` as soon as it is
  available. `cb` may be called synchronously if the selector's value is already
  known."
  ([graph-manager cb selector-constructor] (hook-sel graph-manager cb (selector-constructor)))
  ([graph-manager cb selector-constructor a] (hook-sel graph-manager cb (selector-constructor a)))
  ([graph-manager cb selector-constructor a b] (hook-sel graph-manager cb (selector-constructor a b)))
  ([graph-manager cb selector-constructor a b c] (hook-sel graph-manager cb (selector-constructor a b c)))
  ([graph-manager cb selector-constructor a b c d] (hook-sel graph-manager cb (selector-constructor a b c d)))
  ([graph-manager cb selector-constructor a b c d f] (hook-sel graph-manager cb (selector-constructor a b c d f)))
  ([graph-manager cb selector-constructor a b c d f g] (hook-sel graph-manager cb (selector-constructor a b c d f g)))
  ([graph-manager cb selector-constructor a b c d f g h] (hook-sel graph-manager cb (selector-constructor a b c d f g h))))

(defn hook-change
  "Like hook-change-sel, but receives a selector-constructor plus arguments
  instead of a selector."
  ([graph-manager cb selector-constructor] (hook-change-sel graph-manager cb (selector-constructor)))
  ([graph-manager cb selector-constructor a] (hook-change-sel graph-manager cb (selector-constructor a)))
  ([graph-manager cb selector-constructor a b] (hook-change-sel graph-manager cb (selector-constructor a b)))
  ([graph-manager cb selector-constructor a b c] (hook-change-sel graph-manager cb (selector-constructor a b c)))
  ([graph-manager cb selector-constructor a b c d] (hook-change-sel graph-manager cb (selector-constructor a b c d)))
  ([graph-manager cb selector-constructor a b c d f] (hook-change-sel graph-manager cb (selector-constructor a b c d f)))
  ([graph-manager cb selector-constructor a b c d f g] (hook-change-sel graph-manager cb (selector-constructor a b c d f g)))
  ([graph-manager cb selector-constructor a b c d f g h] (hook-change-sel graph-manager cb (selector-constructor a b c d f g h))))

(defn hitch-callback
  "Given a graph, execute fn `body` in a graph transaction context, calling
  fn `cb` with the result of body when available. `body` will be called like
  `(apply body graph-manager extra-args-to-hitch-callback)`. Body may use select! and
  may be called multiple times.

  If body uses `select!` and derefs unavailable values, the exception will
  be caught and body will be called again when the value is available.
  This continues until body returns without throwing an exception."
  ([graph-manager cb body]
    )
  ([graph-manager cb body a]
    )
  ([graph-manager cb body a b]
    )
  ([graph-manager cb body a b c]
    )
  ([graph-manager cb body a b c d]
    )
  ([graph-manager cb body a b c d e]
    )
  ([graph-manager cb body a b c d e f]
    )
  ([graph-manager cb body a b c d e f g]
    ))

(defn apply-commands
  "Issue a list of selector-command pairs to a graph. Selector-command-pairs
   is like `[[Selector [command & command-args]] ,,,]`. Do not rely on returned
   value."
  [graph-manager selector-command-pairs]
  )

;;tx-manager options
(defn dget-sel!
  "Return the value (or `nf` if not yet known) for a selector from graph
  transaction context `tx`."
  [tx-manager selector nf]
  (tx-proto/dget-sel! tx-manager selector nf))

(defn dget!
  "Return the value (or `nf` if not yet known) for a selector-constructor and
  its arguments from graph transaction context `tx`."
  ([tx nf selector-constructor]
   (dget-sel! tx (selector-constructor) nf))
  ([tx nf selector-constructor a]
   (dget-sel! tx (selector-constructor a) nf))
  ([tx nf selector-constructor a b]
   (dget-sel! tx (selector-constructor a b) nf))
  ([tx nf selector-constructor a b c]
   (dget-sel! tx (selector-constructor a b c) nf))
  ([tx nf selector-constructor a b c d]
   (dget-sel! tx (selector-constructor a b c d) nf))
  ([tx nf selector-constructor a b c d e]
   (dget-sel! tx (selector-constructor a b c d e) nf))
  ([tx nf selector-constructor a b c d e f]
   (dget-sel! tx (selector-constructor a b c d e f) nf))
  ([tx nf selector-constructor a b c d e f g]
   (dget-sel! tx (selector-constructor a b c d e f g) nf)))

(defn select-sel!
  "Return a box containing the value for a selector from graph transaction
  context `tx`. The returned box is the same as that returned by `select!`."
  ([tx-manager selector]
   (let [v (dget-sel! tx-manager selector NOT-IN-GRAPH-SENTINEL)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v)))))

(defn select!
  "Return a box containing the value for a selector-constructor and its
  arguments from graph transaction context `tx`. Retrieve the value with deref
  (@). If the value is not yet known, deref will throw an exception which the
  transaction context will catch. You can test if a value is available
  using `(realized? box)`."
  ([tx selector-constructor]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor a)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor a b)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor a b c)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor a b c d)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d e]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d e f]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx selector-constructor a b c d e f g]
   (let [v (dget! tx NOT-IN-GRAPH-SENTINEL selector-constructor a b c d e f g)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v)))))

