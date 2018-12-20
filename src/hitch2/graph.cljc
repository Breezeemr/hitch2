(ns hitch2.graph
  (:require [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.descriptor :as descriptor]
            [hitch2.curator.pin :refer [pin-curator]]
            [hitch2.curator.hook :refer [hook-curator hook-change-curator]]
            [hitch2.curator.hitch-callback :as h-cb :refer [hitch-callback-curator]]
            [hitch2.protocols.tx-manager :as tx-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
            [hitch2.halt :as halt]))



(def positional-dtor descriptor/positional-dtor)

(def ->dtor descriptor/->dtor)

(def descriptor? descriptor/descriptor?)

(defn descriptor-instance? [spec dtor]
  (assert (map? spec) (str "Spec is not a map: " spec))
  (= (:name dtor)
     (:hitch2.descriptor/name spec)))

(defn pin
  "Force a descriptor to remain in the graph even if nothing else depends on it."
  [graph-manager descriptor]
  (graph-proto/-transact! graph-manager pin-curator [:pin descriptor]))

(defn unpin
  "Allow a descriptor to be removed from the graph if nothing else depends on it."
  [graph-manager descriptor]
  (graph-proto/-transact! graph-manager pin-curator [:unpin descriptor]))

(defn hook-sel
  "Call fn `cb` once with the value of `descriptor` in `graph` as soon as it is
  available. `cb` may be called synchronously if the descriptor's value is already
  known."
  [graph-manager cb descriptor]
  (let [graph-value (graph-proto/-get-graph graph-manager)
        val  (get graph-value descriptor NOT-IN-GRAPH-SENTINEL)]
    (if (identical? val NOT-IN-GRAPH-SENTINEL)
      (graph-proto/-transact! graph-manager hook-curator [:hook-subscribe descriptor cb])
      (cb #_graph-manager val)))
  nil)

(defn hook-change-sel
  "Call fn `cb` with the value of `descriptor` in `graph` as soon as it is
  available, and every time the value changes. `cb` may be called synchronously
  if the descriptor's value is already known and the graph supports
  eager descriptor resolution.

  Returns a zero-arg unsubscribe function. After it is called, cb will not
  be called again.

  There is no guarantee that each `cb` call will receive a value not= to the
  previous call's value."
  [graph-manager cb descriptor]
  (graph-proto/-transact! graph-manager hook-change-curator [:hook-change-subscribe descriptor cb])
  (fn [] (graph-proto/-transact! graph-manager hook-change-curator [:hook-change-unsubscribe descriptor cb])))

(defn hook
  "Call fn `cb` once with the value of descriptor returned from
  descriptor-spec and remaining arguments in `graph` as soon as it is
  available. `cb` may be called synchronously if the descriptor's value is already
  known."
  ([graph-manager cb descriptor-spec] (hook-sel graph-manager cb (positional-dtor descriptor-spec)))
  ([graph-manager cb descriptor-spec a] (hook-sel graph-manager cb (positional-dtor descriptor-spec a)))
  ([graph-manager cb descriptor-spec a b] (hook-sel graph-manager cb (positional-dtor descriptor-spec a b)))
  ([graph-manager cb descriptor-spec a b c] (hook-sel graph-manager cb (positional-dtor descriptor-spec a b c)))
  ([graph-manager cb descriptor-spec a b c d] (hook-sel graph-manager cb (positional-dtor descriptor-spec a b c d)))
  ([graph-manager cb descriptor-spec a b c d f] (hook-sel graph-manager cb (positional-dtor descriptor-spec a b c d f)))
  ([graph-manager cb descriptor-spec a b c d f g] (hook-sel graph-manager cb (positional-dtor descriptor-spec a b c d f g)))
  ([graph-manager cb descriptor-spec a b c d f g h] (hook-sel graph-manager cb (positional-dtor descriptor-spec a b c d f g h))))

(defn hook-change
  "Like hook-change-sel, but receives a descriptor-spec plus arguments
  instead of a descriptor."
  ([graph-manager cb descriptor-spec] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec)))
  ([graph-manager cb descriptor-spec a] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec a)))
  ([graph-manager cb descriptor-spec a b] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec a b)))
  ([graph-manager cb descriptor-spec a b c] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec a b c)))
  ([graph-manager cb descriptor-spec a b c d] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec a b c d)))
  ([graph-manager cb descriptor-spec a b c d f] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec a b c d f)))
  ([graph-manager cb descriptor-spec a b c d f g] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec a b c d f g)))
  ([graph-manager cb descriptor-spec a b c d f g h] (hook-change-sel graph-manager cb (positional-dtor descriptor-spec a b c d f g h))))

(defn hitch-callback
  "Given a graph, execute fn `body` in a graph transaction context, calling
  fn `cb` with the result of body when available. `body` will be called like
  `(apply body graph-manager extra-args-to-hitch-callback)`. Body may use select! and
  may be called multiple times.

  If body uses `select!` and derefs unavailable values, the exception will
  be caught and body will be called again when the value is available.
  This continues until body returns without throwing an exception."
  ([graph-manager cb halt-fn]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx))
                   cb)
   nil)
  ([graph-manager cb halt-fn a]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx a))
                   cb)
   nil)
  ([graph-manager cb halt-fn a b]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx a b))
                   cb)
   nil)
  ([graph-manager cb halt-fn a b c]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx a b c))
                   cb)
   nil)
  ([graph-manager cb halt-fn a b c d]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx a b c d))
                   cb)
   nil)
  ([graph-manager cb halt-fn a b c d e]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx a b c d e))
                   cb)
   nil)
  ([graph-manager cb halt-fn a b c d e f]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx a b c d e f))
                   cb)
   nil)
  ([graph-manager cb halt-fn a b c d e f g]
   (h-cb/first-run graph-manager
                   (fn [rtx] (halt-fn rtx a b c d e f g))
                   cb)
   nil))

(defn apply-commands
  "Issue a list of descriptor-command pairs to a graph. descriptor-command-pairs
   is like `[[descriptor [command & command-args]] ,,,]`. Do not rely on returned
   value."
  [graph-manager descriptor-command-pairs]
  (graph-proto/-transact-commands! graph-manager descriptor-command-pairs)
  )

;;tx-manager options
(defn dget-sel!
  "Return the value (or `nf` if not yet known) for a descriptor from graph
  transaction context `tx`."
  [tx-manager descriptor nf]
  (tx-proto/dget-sel! tx-manager descriptor nf))

(defn dget!
  "Return the value (or `nf` if not yet known) for a descriptor-spec and
  its arguments from graph transaction context `tx`."
  ([tx-manager nf descriptor-spec]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec)
       (positional-dtor descriptor-spec)) nf))
  ([tx-manager nf descriptor-spec a]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec a)
       (positional-dtor descriptor-spec a)) nf))
  ([tx-manager nf descriptor-spec a b]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec a b)
       (positional-dtor descriptor-spec a b)) nf))
  ([tx-manager nf descriptor-spec a b c]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec a b c)
       (positional-dtor descriptor-spec a b c)) nf))
  ([tx-manager nf descriptor-spec a b c d]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec a b c d)
       (positional-dtor descriptor-spec a b c d)) nf))
  ([tx-manager nf descriptor-spec a b c d e]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec a b c d e)
       (positional-dtor descriptor-spec a b c d e)) nf))
  ([tx-manager nf descriptor-spec a b c d e f]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec a b c d e f)
       (positional-dtor descriptor-spec a b c d e f)) nf))
  ([tx-manager nf descriptor-spec a b c d e f g]
   (dget-sel! tx-manager
     (if (fn? descriptor-spec)
       (descriptor-spec a b c d e f g)
       (positional-dtor descriptor-spec a b c d e f g)) nf)))

(defn select-sel!
  "Return a box containing the value for a descriptor from graph transaction
  context `tx`. The returned box is the same as that returned by `select!`."
  ([tx-manager descriptor]
   (let [v (dget-sel! tx-manager descriptor NOT-IN-GRAPH-SENTINEL)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v)))))

(defn select!
  "Return a box containing the value for a descriptor-spec and its
  arguments from graph transaction context `tx`. Retrieve the value with deref
  (@). If the value is not yet known, deref will throw an exception which the
  transaction context will catch. You can test if a value is available
  using `(realized? box)`."
  ([tx-manager descriptor-spec]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx-manager descriptor-spec a]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec a)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx-manager descriptor-spec a b]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec a b)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx-manager descriptor-spec a b c]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec a b c)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx-manager descriptor-spec a b c d]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec a b c d)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx-manager descriptor-spec a b c d e]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec a b c d e)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx-manager descriptor-spec a b c d e f]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec a b c d e f)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v))))
  ([tx-manager descriptor-spec a b c d e f g]
   (let [v (dget! tx-manager NOT-IN-GRAPH-SENTINEL descriptor-spec a b c d e f g)]
     (if (identical? v NOT-IN-GRAPH-SENTINEL)
       halt/halt-box
       (halt/select-box v)))))

(defn snapshot [graph]
  (graph-proto/-get-graph graph))
