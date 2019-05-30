(ns hitch2.def.curator
  (:require [clojure.spec.alpha :as s]))

;(def descriptor? any?)
;(s/def ::state any?)
;(s/def ::change-focus (s/map-of descriptor? boolean?))
;(s/def ::set-projections (s/map-of descriptor? any?))
;(s/def ::async-effects (s/coll-of any?))
;(s/def ::sync-effects (s/coll-of any?))
;
;(s/def ::curator-state
;  (s/keys :opt-un
;    [::state ::change-focus ::set-projections
;     ::async-effects ::sync-effects]))
;
(defrecord curator-state [state change-focus set-projections
                          async-effects outbox])

(def initial-curator-state (->curator-state nil {} {} [] {}))
;
;:hitch2.def.curator/init
;:hitch2.def.curator/tx-init
;:hitch2.def.curator/curation-changes
;:hitch2.def.curator/observed-value-changes
;:hitch2.def.curator/apply-command
;:hitch2.def.curator/flush-tx
;:hitch2.def.curator/finalize
;:hitch2.def.curator/de-init
