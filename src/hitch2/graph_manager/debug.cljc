(ns hitch2.graph-manager.debug
  (:require [hitch2.protocols.graph-manager :as gm]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]))

(defn not-loaded-descriptors
  [gm descriptor]
  (let [dependencies (gm/-observes gm descriptor)
        snapshot     (gm/-get-graph gm)
        resolver     (gm/-get-resolver gm)]
    (when-not (identical? dependencies NOT-IN-GRAPH-SENTINEL)
      (into {}
            (keep (fn [dep]
                    (let [dep-value (get snapshot dep NOT-FOUND-SENTINEL)]
                      (when (identical? dep-value NOT-FOUND-SENTINEL)
                        (let [impl (resolver dep)]
                          (case (:hitch2.descriptor.impl/kind impl)
                            :hitch2.descriptor.kind/curator
                            [:curator dep]
                            (:hitch2.descriptor.kind/var :hitch2.descriptor.kind/halting)
                            [dep (not-loaded-descriptors gm dep)]))))))
            dependencies))))
