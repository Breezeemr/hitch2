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

