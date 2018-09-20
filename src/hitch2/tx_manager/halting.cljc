(ns hitch2.tx-manager.halting
  (:require [hitch2.protocols.tx-manager :as tx-manager]))


(deftype halting-manager [graph-value #?(:cljs    ^:mutable requests
                                        :default ^:unsynchronized-mutable requests)]
  tx-manager/IDependTrack
  (dget-sel!  [this data-selector nf]
    (set! requests (conj! requests)))
  (finish-tx! [this]
    (let [reqs (persistent! requests)]
      (set! requests (transient #{}))
      reqs)))
