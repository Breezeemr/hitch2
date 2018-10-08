(ns hitch2.protocols.machine
  (:require [clojure.spec.alpha :as s]))

(def selector? any?)
(s/def ::state any?)
(s/def ::change-focus (s/map-of selector? boolean?))
(s/def ::project-values (s/map-of selector? any?))
(s/def ::async-effects (s/coll-of any?))
(s/def ::sync-effects (s/coll-of any?))

(s/def ::machine-state
  (s/keys :opt-un
    [::state ::change-focus ::project-values
     ::async-effects ::sync-effects]))

(defrecord machine-state [state change-focus project-values
                       async-effects sync-effects])

(def initial-machine-state (->machine-state nil {} {} [] []))

;; ability to supply initial state
(defprotocol Init
  (-initialize [machine-instance machine-selector]))

(extend-protocol Init
  #?(:clj  Object
     :cljs default)
  (-initialize [machine-instance machine-selector]
    initial-machine-state))

;; have handles to stateful objects. for instance, if you get
;; deinitted, need to cancel the existing xhr's that are in flight.
(defprotocol Deinit
  (-uninitialize [machine-instance machine-selector ^machine-state node]
    node))

(extend-protocol Deinit
  #?(:clj  Object
     :cljs default)
  (-uninitialize [machine-instance machine-selector ^machine-state node]
    nil))

;; people depending on this machine changed
(defprotocol ChildChanges
  (-child-changes [machine-instance machine-selector graph-value ^machine-state node children parents children-added children-removed]))

;; things you depended on value's changed
(defprotocol ParentChanges
  (-parent-value-changes [machine-instance machine-selector graph-value ^machine-state node children parents parent-selectors]))
(defprotocol Commandable
  (-apply-command [machine-instance machine-selector graph-value ^machine-state node children parents command]))

;; batching use cases (really wanted on the server). any time in this tx
(defprotocol InitForTX
  (-init-tx [machine-instance machine-selector graph-value ^machine-state node children parents]))

(extend-protocol InitForTX
  #?(:clj  Object
     :cljs default)
  (-init-tx [machine-instance machine-selector graph-value ^machine-state node children parents]
    node))

(defprotocol FlushForTX
  (-flush-tx [machine-instance machine-selector graph-value ^machine-state node children parents]))

(extend-protocol FlushForTX
  #?(:clj  Object
     :cljs default)
  (-flush-tx [machine-instance machine-selector graph-value ^machine-state node children parents]
    node))

(defprotocol FinalizeForTX
  (-finalize [machine-instance machine-selector graph-value ^machine-state node children parents]))

(extend-protocol FinalizeForTX
  #?(:clj  Object
     :cljs default)
  (-finalize [machine-instance machine-selector graph-value ^machine-state node children parents]
    node))
