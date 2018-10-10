(ns hitch2.protocols.curator
  (:require [clojure.spec.alpha :as s]))

(def selector? any?)
(s/def ::state any?)
(s/def ::change-focus (s/map-of selector? boolean?))
(s/def ::set-projections (s/map-of selector? any?))
(s/def ::async-effects (s/coll-of any?))
(s/def ::sync-effects (s/coll-of any?))

(s/def ::curator-state
  (s/keys :opt-un
    [::state ::change-focus ::set-projections
     ::async-effects ::sync-effects]))

(defrecord curator-state [state change-focus set-projections
                       async-effects sync-effects])

(def initial-curator-state (->curator-state nil {} {} [] []))

;; ability to supply initial state
(defprotocol Init
  (-initialize [machine-instance machine-selector]))

(extend-protocol Init
  #?(:clj  Object
     :cljs default)
  (-initialize [machine-instance machine-selector]
    initial-curator-state)
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-initialize [machine-instance machine-selector]
         (if-some [init (::init machine-instance)]
           (init machine-selector)
           initial-curator-state))
       cljs.core/PersistentArrayMap
       (-initialize [machine-instance machine-selector]
         (if-some [init (::init machine-instance)]
           (init machine-selector)
           initial-curator-state))]
      :clj
      [clojure.lang.APersistentMap
       (-initialize [machine-instance machine-selector]
         (if-some [init (::init machine-instance)]
           (init machine-selector)
           initial-curator-state))]))

;; have handles to stateful objects. for instance, if you get
;; deinitted, need to cancel the existing xhr's that are in flight.
(defprotocol Deinit
  (-uninitialize [machine-instance machine-selector ^curator-state node]
    node))

(extend-protocol Deinit
  #?(:clj  Object
     :cljs default)
  (-uninitialize [machine-instance machine-selector ^curator-state node]
    nil)
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-uninitialize [machine-instance machine-selector ^curator-state node]
         (if-some [deinit (::deinit machine-instance)]
           (deinit machine-selector node)
           node))
       cljs.core/PersistentArrayMap
       (-uninitialize [machine-instance machine-selector ^curator-state node]
         (if-some [deinit (::deinit machine-instance)]
           (deinit machine-selector node)
           node))]
      :clj
      [clojure.lang.APersistentMap
       (-uninitialize [machine-instance machine-selector ^curator-state node]
         (if-some [deinit (::deinit machine-instance)]
           (deinit machine-selector node)
           node))]))

;; people depending on this curator changed
(defprotocol ChildChanges
  (-child-changes [machine-instance machine-selector graph-value ^curator-state node children-added children-removed]))

(extend-protocol ChildChanges
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-child-changes [machine-instance machine-selector graph-value ^curator-state node children-added children-removed]
         (if-some [curation-changes (::curation-changes machine-instance)]
           (curation-changes machine-selector graph-value node  children-added children-removed)
           (assert false)))
       cljs.core/PersistentArrayMap
       (-child-changes [machine-instance machine-selector graph-value ^curator-state node children-added children-removed]
         (if-some [curation-changes (::curation-changes machine-instance)]
           (curation-changes machine-selector graph-value node children-added children-removed)
           (assert false)))]
      :clj
      [clojure.lang.APersistentMap
       (-child-changes [machine-instance machine-selector graph-value ^curator-state node children-added children-removed]
         (if-some [curation-changes (::curation-changes machine-instance)]
           (curation-changes machine-selector graph-value node children-added children-removed)
           (assert false)))]))
;; things you depended on value's changed
(defprotocol ParentChanges
  (-parent-value-changes [machine-instance machine-selector graph-value ^curator-state node parent-selectors]))
(extend-protocol ParentChanges
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-parent-value-changes [machine-instance machine-selector graph-value ^curator-state node parent-selectors]
         (if-some [observed-value-changes (::observed-value-changes machine-instance)]
           (observed-value-changes machine-selector graph-value node  parent-selectors)
           (assert false)))
       cljs.core/PersistentArrayMap
       (-parent-value-changes [machine-instance machine-selector graph-value ^curator-state node parent-selectors]
         (if-some [observed-value-changes (::observed-value-changes machine-instance)]
           (observed-value-changes machine-selector graph-value node parent-selectors)
           (assert false)))]
      :clj
      [clojure.lang.APersistentMap
       (-parent-value-changes [machine-instance machine-selector graph-value ^curator-state node parent-selectors]
         (if-some [observed-value-changes (::observed-value-changes machine-instance)]
           (observed-value-changes machine-selector graph-value node parent-selectors)
           (assert false)))]))

(defprotocol Commandable
  (-apply-command [machine-instance machine-selector graph-value ^curator-state node command]))
(extend-protocol Commandable
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-apply-command [machine-instance machine-selector graph-value ^curator-state node command]
         (if-some [apply-command (::apply-command machine-instance)]
           (apply-command machine-selector graph-value node  command)
           (assert false)))
       cljs.core/PersistentArrayMap
       (-apply-command [machine-instance machine-selector graph-value ^curator-state node command]
         (if-some [apply-command (::apply-command machine-instance)]
           (apply-command machine-selector graph-value node command)
           (assert false)))]
      :clj
      [clojure.lang.APersistentMap
       (-apply-command [machine-instance machine-selector graph-value ^curator-state node command]
         (if-some [apply-command (::apply-command machine-instance)]
           (apply-command machine-selector graph-value node command)
           (assert false)))]))

;; batching use cases (really wanted on the server). any time in this tx
(defprotocol InitForTX
  (-init-tx [machine-instance machine-selector graph-value ^curator-state node]))

(extend-protocol InitForTX
  #?(:clj  Object
     :cljs default)
  (-init-tx [machine-instance machine-selector graph-value ^curator-state node]
    node)
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-init-tx [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [tx-init (::tx-init machine-instance)]
           (tx-init machine-selector graph-value node)
           node))
       cljs.core/PersistentArrayMap
       (-init-tx [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [tx-init (::tx-init machine-instance)]
           (tx-init machine-selector graph-value node)
           node))]
      :clj
      [clojure.lang.APersistentMap
       (-init-tx [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [tx-init (::tx-init machine-instance)]
           (tx-init machine-selector graph-value node)
           node))]))

(defprotocol FlushForTX
  (-flush-tx [machine-instance machine-selector graph-value ^curator-state node]))

(extend-protocol FlushForTX
  #?(:clj  Object
     :cljs default)
  (-flush-tx [machine-instance machine-selector graph-value ^curator-state node]
    node)
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-flush-tx [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [flush-tx (::flush-tx machine-instance)]
           (flush-tx machine-selector graph-value node)
           node))
       cljs.core/PersistentArrayMap
       (-flush-tx [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [flush-tx (::flush-tx machine-instance)]
           (flush-tx machine-selector graph-value node)
           node))]
      :clj
      [clojure.lang.APersistentMap
       (-flush-tx [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [flush-tx (::flush-tx machine-instance)]
           (flush-tx machine-selector graph-value node)
           node))]))

(defprotocol FinalizeForTX
  (-finalize [machine-instance machine-selector graph-value ^curator-state node]))

(extend-protocol FinalizeForTX
  #?(:clj  Object
     :cljs default)
  (-finalize [machine-instance machine-selector graph-value ^curator-state node]
    node)
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-finalize [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [finalize (::finalize machine-instance)]
           (finalize machine-selector graph-value node)
           node))
       cljs.core/PersistentArrayMap
       (-finalize [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [finalize (::finalize machine-instance)]
           (finalize machine-selector graph-value node)
           node))]
      :clj
      [clojure.lang.APersistentMap
       (-finalize [machine-instance machine-selector graph-value ^curator-state node]
         (if-some [finalize (::finalize machine-instance)]
           (finalize machine-selector graph-value node)
           node))]))
