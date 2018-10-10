(ns hitch2.protocols.curator
  (:require [clojure.spec.alpha :as s]))

(def selector? any?)
(s/def ::state any?)
(s/def ::change-parent (s/map-of selector? boolean?))
(s/def ::reset-vars (s/map-of selector? any?))
(s/def ::async-effects (s/coll-of any?))
(s/def ::sync-effects (s/coll-of any?))

(s/def ::curator-state
  (s/keys :opt-un
    [::state ::change-parent ::reset-vars
     ::async-effects ::sync-effects]))

(defrecord curator-state [state change-parent reset-vars
                       async-effects sync-effects])

(def initial-curator-state (->curator-state nil {} {} [] []))

;; ability to supply initial state
(defprotocol Init
  (-initialize [machine-instance machine-selector]))

(extend-protocol Init
  #?(:clj  Object
     :cljs default)
  (-initialize [machine-instance machine-selector]
    initial-curator-state))

;; have handles to stateful objects. for instance, if you get
;; deinitted, need to cancel the existing xhr's that are in flight.
(defprotocol Deinit
  (-uninitialize [machine-instance machine-selector ^curator-state node]
    node))

(extend-protocol Deinit
  #?(:clj  Object
     :cljs default)
  (-uninitialize [machine-instance machine-selector ^curator-state node]
    nil))

;; people depending on this curator changed
(defprotocol ChildChanges
  (-child-changes [machine-instance machine-selector graph-value ^curator-state node children-added children-removed]))

;; things you depended on value's changed
(defprotocol ParentChanges
  (-parent-value-changes [machine-instance machine-selector graph-value ^curator-state node parent-selectors]))
(defprotocol Commandable
  (-apply-command [machine-instance machine-selector graph-value ^curator-state node command]))

;; batching use cases (really wanted on the server). any time in this tx
(defprotocol InitForTX
  (-init-tx [machine-instance machine-selector graph-value ^curator-state node]))

(extend-protocol InitForTX
  #?(:clj  Object
     :cljs default)
  (-init-tx [machine-instance machine-selector graph-value ^curator-state node]
    node))

(defprotocol FlushForTX
  (-flush-tx [machine-instance machine-selector graph-value ^curator-state node]))

(extend-protocol FlushForTX
  #?(:clj  Object
     :cljs default)
  (-flush-tx [machine-instance machine-selector graph-value ^curator-state node]
    node))

(defprotocol FinalizeForTX
  (-finalize [machine-instance machine-selector graph-value ^curator-state node]))

(extend-protocol FinalizeForTX
  #?(:clj  Object
     :cljs default)
  (-finalize [machine-instance machine-selector graph-value ^curator-state node]
    node))
