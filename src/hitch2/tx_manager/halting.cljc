(ns hitch2.tx-manager.halting
  (:require [hitch2.protocols.tx-manager :as tx-manager]
            [hitch2.halt :as halt]))


(deftype HaltingManager [graph-value #?(:cljs    ^:mutable requests
                                        :default ^:unsynchronized-mutable requests)]
  tx-manager/IDependTrack
  (dget-sel!  [this data-descriptor nf]
    (set! requests (conj! requests data-descriptor))
    (get graph-value data-descriptor nf))
  (finish-tx! [this]
    (let [reqs (persistent! requests)]
      (set! requests (transient #{}))
      reqs)))

(defn halting-manager [graph-value]
  (HaltingManager. graph-value (transient #{})))
