(ns hitch2.protocols.machine
  (:require [clojure.spec.alpha :as s]))

(def selector? any?)
(s/def ::state any?)
(s/def ::change-parent (s/map-of selector? boolean?))
(s/def ::reset-vars (s/map-of selector? any?))
(s/def ::async-effects (s/coll-of any?))
(s/def ::sync-effects (s/coll-of any?))

(s/def ::node-state
  (s/keys :opt-un
    [::state ::change-parent ::reset-vars
     ::async-effects ::sync-effects]))

(defrecord node-state [state change-parent reset-vars
                       async-effects sync-effects])

(def initial-node (->node-state {} nil nil [] []))

(defprotocol Init
  (-initialize [machine-instance]))

(extend-protocol Init
  #?(:clj  Object
     :cljs default)
  (-initialize [machine-instance]
    (->node-state nil nil nil nil nil)))

(defprotocol Deinit
  (-uninitialize [machine-instance ^node-state node]
    node))

(extend-protocol Deinit
  #?(:clj  Object
     :cljs default)
  (-uninitialize [machine-instance ^node-state node]
    nil))

(defprotocol ChildChanges
  (-child-changes [machine-instance graph-value ^node-state node children parents children-added children-removed]))
(defprotocol ParentChanges
  (-parent-value-changes [machine-instance graph-value ^node-state node children parents parent-selectors]))
(defprotocol Commandable
  (-apply-command [machine-instance graph-value ^node-state node children parents command]))

(defprotocol InitForTX
  (-init-tx [machine-instance graph-value ^node-state node children parents]))

(extend-protocol InitForTX
  #?(:clj  Object
     :cljs default)
  (-init-tx [machine-instance graph-value ^node-state node children parents]
    node))

(defprotocol FlushForTX
  (-flush-tx [machine-instance graph-value ^node-state node children parents]))

(extend-protocol FlushForTX
  #?(:clj  Object
     :cljs default)
  (-flush-tx [machine-instance graph-value ^node-state node children parents]
    node))

(defprotocol FinalizeForTX
  (-finalize [machine-instance graph-value ^node-state node children parents]))

(extend-protocol FinalizeForTX
  #?(:clj  Object
     :cljs default)
  (-finalize [machine-instance graph-value ^node-state node children parents]
    node))
